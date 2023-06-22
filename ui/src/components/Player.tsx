import { GraphicEq, Pause, Person, PlayArrow } from '@mui/icons-material'
import { Box, Fab, styled } from '@mui/material'
import { useCallback, useEffect, useState } from 'react'

import type { Feed } from '@/graphql/generated'
import useFeedPresence from '@/hooks/useFeedPresence'
import useIsMobile from '@/hooks/useIsMobile'
import useTimestampFetcher from '@/hooks/useTimestampFetcher'

import DetectionDialog from './DetectionDialog'
import MediaStreamer from './MediaStreamer'

const PlayerContainer = styled('div')`
  .video-js {
    display: none;
  }
`

const StyledButtonContainer = styled('div')`
  border-radius: 2.0625rem;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: flex-start;
`

export default function Player({
  currentFeed,
}: {
  currentFeed?: Pick<Feed, 'slug' | 'nodeName' | 'name' | 'locationPoint'>
}) {
  const [isPlaying, setIsPlaying] = useState(false)
  const [isLoading, setIsLoading] = useState(false)
  const [controls, setControls] = useState({
    play: () => {},
    pause: () => {},
    playPause: () => {},
    getPlayerTime: () => {},
    setVolume: () => {},
  })

  const [debugInfo, setDebugInfo] = useState<{
    playerTime: number
    latencyHistory: number[]
  }>()

  const handleFetcherStop = useCallback(() => {
    setIsPlaying(false)
    setIsLoading(false)
  }, [])

  const { timestamp, hlsURI, awsConsoleUri } = useTimestampFetcher(
    currentFeed?.nodeName,
    {
      onStop: handleFetcherStop,
    }
  )

  const feedPresence = useFeedPresence(currentFeed?.slug)
  const listenerCount = feedPresence?.metas.length ?? 0

  const isMobile = useIsMobile()

  const handleReady = (controls: any) => {
    setIsLoading(false)
    setControls(controls)
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
      {isPlaying && (
        <DetectionDialog
          isPlaying={isPlaying}
          feed={currentFeed}
          timestamp={timestamp}
          getPlayerTime={controls.getPlayerTime}
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
      <PlayerContainer sx={{ mx: 2 }}>
        <StyledButtonContainer>
          <Fab
            color="base"
            sx={{
              // set hover color manually because custom colors are broken for Fab
              // see https://github.com/mui/material-ui/issues/31063
              '&:hover': {
                backgroundColor: 'base.light',
              },
            }}
            onClick={controls.playPause}
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
        </StyledButtonContainer>
        {hlsURI && (
          <MediaStreamer
            src={hlsURI}
            key={hlsURI}
            autoplay={true}
            onReady={handleReady}
            onLoading={() => setIsLoading(true)}
            onPlaying={() => {
              setIsLoading(false)
              setIsPlaying(true)
            }}
            onPaused={() => {
              setIsLoading(false)
              setIsPlaying(false)
            }}
            onLatencyUpdate={(newestLatency: number, playerTime: number) =>
              setDebugInfo((prevDebugInfo) => ({
                playerTime: playerTime,
                latencyHistory: (prevDebugInfo?.latencyHistory ?? []).concat(
                  newestLatency
                ),
              }))
            }
          />
        )}
      </PlayerContainer>
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
