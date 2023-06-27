import { GraphicEq, Pause, Person, PlayArrow } from '@mui/icons-material'
import { Box, Fab } from '@mui/material'
import dynamic from 'next/dynamic'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import VideoJSPlayer from 'video.js/dist/types/player'

import DetectionDialog from '@/components/DetectionDialog'
import type { Feed } from '@/graphql/generated'
import useFeedPresence from '@/hooks/useFeedPresence'
import useIsMobile from '@/hooks/useIsMobile'
import useTimestampFetcher from '@/hooks/useTimestampFetcher'

// dynamically import VideoJS to speed up initial page load
const VideoJS = dynamic(() => import('@/components/VideoJS'))

export default function Player({
  currentFeed,
}: {
  currentFeed?: Pick<
    Feed,
    'id' | 'slug' | 'nodeName' | 'name' | 'locationPoint'
  >
}) {
  const [isPlaying, setIsPlaying] = useState(false)
  const [_isLoading, setIsLoading] = useState(false)
  const playerRef = useRef<VideoJSPlayer | null>(null)

  const [_debugInfo, setDebugInfo] = useState<{
    playerTime: number
    latencyHistory: number[]
  }>()

  const handleFetcherStop = useCallback(() => {
    setIsPlaying(false)
    setIsLoading(false)
  }, [])

  const { timestamp, hlsURI } = useTimestampFetcher(currentFeed?.nodeName, {
    onStop: handleFetcherStop,
  })

  const feedPresence = useFeedPresence(currentFeed?.slug)
  const listenerCount = feedPresence?.metas.length ?? 0

  const isMobile = useIsMobile()

  const playerOptions = useMemo(
    () =>
      hlsURI
        ? {
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
                src: hlsURI,
                type: 'application/x-mpegurl',
              },
            ],
          }
        : {},
    [hlsURI]
  )

  const handleReady = useCallback((player: VideoJSPlayer) => {
    playerRef.current = player

    setIsLoading(false)

    player.on('playing', () => {
      setIsLoading(false)
      setIsPlaying(true)
    })
    player.on('pause', () => {
      setIsLoading(false)
      setIsPlaying(false)
    })
    player.on('waiting', () => setIsLoading(true))
  }, [])

  const handlePlayPause = () => {
    const player = playerRef.current
    if (!player) return
    if (player.paused()) {
      player.play()
    } else {
      player.pause()
    }
  }

  const handleLatencyUpdate = useCallback(
    (newestLatency: number, playerTime: number) =>
      setDebugInfo((prevDebugInfo) => ({
        playerTime: playerTime,
        latencyHistory: (prevDebugInfo?.latencyHistory ?? []).concat(
          newestLatency
        ),
      })),
    []
  )

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
      {isPlaying && currentFeed && timestamp && (
        <DetectionDialog
          isPlaying={isPlaying}
          feed={currentFeed}
          timestamp={timestamp}
          getPlayerTime={playerRef.current?.currentTime}
          listenerCount={listenerCount}
        >
          <Fab
            variant="extended"
            size="large"
            color="secondary"
            sx={{
              position: 'absolute',
              bottom: 100,
              left: 0,
              right: 0,
              margin: 'auto',
              maxWidth: 'max-content',

              // style to look like outlined button
              backgroundColor: 'white',
              color: 'primary.main',
              borderColor: 'primary.main',
              borderStyle: 'solid',
              borderWidth: '2px',
              '&:hover': {
                backgroundColor: 'primary.main',
                color: 'white',
              },
            }}
          >
            <Box
              sx={{
                display: 'flex',
                justifyContent: 'center',
                alignItems: 'center',
              }}
            >
              <GraphicEq sx={{ mr: 1 }} />
              Report sound
            </Box>
          </Fab>
        </DetectionDialog>
      )}
      <Box
        sx={{
          mx: 2,
          '& .video-js': {
            display: 'none',
          },
        }}
      >
        <Fab
          color="base"
          sx={{
            // set hover color manually because custom colors are broken for Fab
            // see https://github.com/mui/material-ui/issues/31063
            '&:hover': {
              backgroundColor: 'base.light',
            },
          }}
          onClick={handlePlayPause}
        >
          {!isPlaying && (
            <PlayArrow
              className="icon"
              fontSize="large"
              // onClick={() =>
              //   analyticsEvents.stream.started(currentFeed.slug)
              // }
            />
          )}
          {isPlaying && (
            <Pause
              className="icon"
              fontSize="large"
              // onClick={() =>
              //   analyticsEvents.stream.paused(currentFeed.slug)
              // }
            />
          )}
        </Fab>
        <VideoJS options={playerOptions} onReady={handleReady} />
      </Box>
      <Box
        sx={{
          mx: 2,
          display: 'flex',
          alignItems: 'center',
        }}
      >
        {currentFeed && (
          <>
            <Person sx={{ mr: 1 }} />
            {listenerCount}
          </>
        )}
      </Box>
      <Box sx={{ mx: 2 }}>
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
