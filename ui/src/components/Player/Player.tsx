import { Box } from '@mui/material'
import dynamic from 'next/dynamic'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'

import type { Feed } from '@/graphql/generated'
import useFeedPresence from '@/hooks/useFeedPresence'
import useIsMobile from '@/hooks/useIsMobile'
import useTimestampFetcher from '@/hooks/useTimestampFetcher'

import DetectionButton from './DetectionButton'
import DetectionDialog from './DetectionDialog'
import ListenerCount from './ListenerCount'
import PlayPauseButton from './PlayPauseButton'
import { type VideoJSPlayer } from './VideoJS'

// dynamically import VideoJS to speed up initial page load
const VideoJS = dynamic(() => import('./VideoJS'))

export default function Player({
  currentFeed,
}: {
  currentFeed?: Pick<
    Feed,
    'id' | 'slug' | 'nodeName' | 'name' | 'locationPoint'
  >
}) {
  const [isPlaying, setIsPlaying] = useState(false)
  const [isLoading, setIsLoading] = useState(false)
  const [isError, setIsError] = useState(false)
  const playerRef = useRef<VideoJSPlayer | null>(null)

  const resetPlayerStatus = useCallback(() => {
    setIsPlaying(false)
    setIsLoading(false)
    setIsError(false)
  }, [])

  const { timestamp, hlsURI } = useTimestampFetcher(currentFeed?.nodeName, {
    onStop: resetPlayerStatus,
  })

  const feedPresence = useFeedPresence(currentFeed?.slug)
  const listenerCount = feedPresence?.metas.length ?? 0

  const isMobile = useIsMobile()

  const playerOptions = useMemo(
    () => ({
      autoplay: true,
      flash: {
        hls: {
          overrideNative: true,
        },
      },
      html5: {
        hls: {
          overrideNative: true,
        },
      },
      sources: [
        {
          src: hlsURI ?? '',
          type: 'application/x-mpegurl',
        },
      ],
    }),
    [hlsURI]
  )

  const handleReady = useCallback(
    (player: VideoJSPlayer) => {
      playerRef.current = player

      resetPlayerStatus()

      player.on('playing', () => {
        resetPlayerStatus()
        setIsPlaying(true)
      })
      player.on('pause', () => {
        resetPlayerStatus()
      })
      player.on('waiting', () => setIsLoading(true))
      player.on('error', () => setIsError(true))
    },
    [resetPlayerStatus]
  )

  const handlePlayPause = async () => {
    const player = playerRef.current

    if (isError) {
      resetPlayerStatus()
      return
    }

    if (!player) {
      setIsError(true)
      return
    }

    try {
      if (!isPlaying) {
        await player.play()
      } else {
        await player.pause()
      }
    } catch (e) {
      console.error(e)
      setIsError(true)
    }
  }

  useEffect(() => {
    if (process.env.NODE_ENV === 'development' && hlsURI) {
      console.log(`New stream instance: ${hlsURI}`)
    }
  }, [hlsURI])

  return (
    <Box
      sx={{
        minHeight: 80,
        color: 'base.contrastText',
        backgroundColor: 'base.main',
        ...(isMobile && { mb: (theme) => theme.spacing(8) }),
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        position: 'relative',
      }}
    >
      <Box display="none">
        <VideoJS options={playerOptions} onReady={handleReady} />
      </Box>
      {isPlaying && currentFeed && timestamp && (
        <DetectionDialog
          isPlaying={isPlaying}
          feed={currentFeed}
          timestamp={timestamp}
          getPlayerTime={() => playerRef.current?.currentTime()}
          listenerCount={listenerCount}
        >
          <DetectionButton />
        </DetectionDialog>
      )}
      <Box mx={2}>
        <PlayPauseButton
          isPlaying={isPlaying}
          isLoading={isLoading}
          isError={isError}
          onClick={handlePlayPause}
        />
      </Box>
      <Box mx={2}>{currentFeed && <ListenerCount count={listenerCount} />}</Box>
      <Box mx={2}>
        {currentFeed
          ? `${currentFeed.name} - ${currentFeed.nodeName}`
          : 'Player: no feed selected'}
      </Box>
      <Box sx={{ mx: 4, flexGrow: 1, textAlign: 'end' }}>
        {currentFeed &&
          `${currentFeed.locationPoint.coordinates[0]}, ${currentFeed.locationPoint.coordinates[1]}`}
      </Box>
    </Box>
  )
}
