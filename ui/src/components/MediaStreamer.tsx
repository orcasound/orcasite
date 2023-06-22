import 'videojs-offset'

import { useEffect, useRef } from 'react'
import videojs from 'video.js'
import Player from 'video.js/dist/types/player'

type PlayerWithOffset = Player & {
  offset: (offset: {
    start: number
    end: number
    restart_beginning: boolean
  }) => void
  // TODO: why aren't these in video.js player type signature?
  readyState: () => number
  seekable: () => TimeRanges
}

type MediaStreamerProps = {
  src: string
  autoplay?: boolean
  startOffset?: number
  endOffset?: number
  callbacks: {
    onReady?: (controls: MediaStreamerControls) => void
    onPlaying?: () => void
    onPaused?: () => void
    onLoading?: () => void
    onTimeUpdate?: (currentTime: number) => void
    onLatencyUpdate?: (latency: number, currentTime: number) => void
  }
}

export type MediaStreamerControls = {
  play: () => void
  pause: () => void
  playPause: () => void
  getPlayerTime: () => number | undefined
  setPlayerTime: (time: number) => void
  seekToLive: (secondsFromLive?: number) => void
  rewind: (seconds: number) => void
  getLatency: () => number | undefined
  setVolume: (volume: number) => void
}

export default function MediaStreamer({
  src,
  autoplay = false,
  startOffset,
  endOffset,
  callbacks,
}: MediaStreamerProps) {
  const audioNodeRef = useRef<HTMLAudioElement>(null)
  const playerRef = useRef<PlayerWithOffset>()

  useEffect(() => {
    let latencyUpdateInterval: NodeJS.Timeout

    const getControls = (player: PlayerWithOffset) => {
      const controls: MediaStreamerControls = {
        play: () => player.play(),
        pause: () => player.pause(),
        playPause: () => {
          if (player.paused()) {
            controls.play()
          } else {
            controls.pause()
          }
        },
        getPlayerTime: () => player.currentTime(),
        setPlayerTime: (time) => player.currentTime(time),
        seekToLive: (secondsFromLive = 30) => {
          if (player.readyState() > 0) {
            player.currentTime(player.seekable().end(0) - secondsFromLive)
          }
        },
        rewind: (seconds) => player.currentTime(player.currentTime() - seconds),
        getLatency: () =>
          player.seekable().length > 0
            ? player.seekable().end(0) - player.currentTime()
            : 0,
        setVolume: (volume) => player.volume(volume / 100),
      }
      return controls
    }

    const setupPlayer = () => {
      const MAX_LATENCY = 300
      // const INITIAL_REWIND_AMOUNT = 90
      const RETRY_REWIND_AMOUNT = 30

      const options = {
        hls: {
          overrideNative: true,
        },
      }

      if (!audioNodeRef.current) {
        console.error('no audioNode set')
        return
      }

      const player = videojs(audioNodeRef.current, {
        autoplay: autoplay,
        flash: options,
        html5: options,
        sources: [
          {
            src: src,
            type: 'application/x-mpegurl',
          },
        ],
      }) as PlayerWithOffset

      playerRef.current = player

      if (startOffset && endOffset) {
        player.offset({
          start: startOffset,
          end: endOffset,
          restart_beginning: true,
        })
      }

      player.ready(() => {
        const controls = getControls(player)
        callbacks.onReady?.(controls)
        player.tech().on('retryplaylist', (_e: Event) => {
          const latency = controls.getLatency()
          if (latency && latency < MAX_LATENCY) {
            controls.rewind(RETRY_REWIND_AMOUNT)
          }
        })
        latencyUpdateInterval = setInterval(() => {
          callbacks.onLatencyUpdate?.(
            getControls(player).getLatency() ?? 0,
            player.currentTime() ?? 0
          )
        }, 1000)
      })

      player.on('playing', () => callbacks.onPlaying?.())
      player.on('pause', () => callbacks.onPaused?.())
      player.on('waiting', () => callbacks.onLoading?.())
      player.on('timeupdate', () =>
        callbacks.onTimeUpdate?.(player.currentTime() ?? 0)
      )
    }

    setupPlayer()
    return () => {
      clearInterval(latencyUpdateInterval)
      if (playerRef.current) {
        getControls(playerRef.current).pause()
        playerRef.current.dispose()
      }
    }
  }, [src, autoplay, startOffset, endOffset, callbacks])

  return (
    <div>
      <audio ref={audioNodeRef} className="video-js" playsInline />
    </div>
  )
}
