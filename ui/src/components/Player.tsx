import { Pause, PlayArrow } from '@mui/icons-material'
import { Box, Fab, styled } from '@mui/material'
import { useState } from 'react'

import type { Feed } from '../generated/types'
import MediaStreamer from './MediaStreamer'

const PlayerContainer = styled('div')`
  .video-js {
    display: none;
  }
`

const StyledButtonContainer = styled('div')<{ active: boolean }>`
  background: ${(props) => (props.active ? '#007166' : 'transparent')};
  box-shadow: ${(props) => (props.active ? '0 0.125rem 0.25rem 0' : 'none')};
  border-radius: 2.0625rem;
  min-width: 21.0625rem;
  max-width: 28.125rem;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: flex-start;
`

export default function Player({
  currentFeed,
  autoplay = false,
}: {
  currentFeed?: Feed
  autoplay: boolean
}) {
  const [isPlaying, setIsPlaying] = useState(false)
  const [isLoading, setIsLoading] = useState(false)
  const [controls, setControls] = useState({ playPause: () => {} })
  const [hlsURI, setHlsURI] = useState(
    'https://s3-us-west-2.amazonaws.com/streaming-orcasound-net/rpi_orcasound_lab/hls/1668565827/live.m3u8'
  )

  const [debugInfo, setDebugInfo] = useState<{
    playerTime: number
    latencyHistory: number[]
  }>()

  const handleReady = (controls: any) => {
    setIsLoading(false)
    setControls(controls)
  }

  return (
    <Box sx={{ minHeight: 80, backgroundColor: 'gray' }}>
      {currentFeed?.nodeName ?? 'Player: no feed selected'}
      <PlayerContainer>
        <Box mt={{ xs: -3.5, sm: -3.5 }} ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}>
          <StyledButtonContainer active={isPlaying}>
            <Fab color="secondary" onClick={controls.playPause}>
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

            {/* {isPlaying && (
              <DetectionDialog
                isPlaying={isPlaying}
                feed={currentFeed}
                timestamp={timestamp}
                getPlayerTime={getPlayerTime}
                listenerCount={this.props.listenerCount}
              />
            )} */}
          </StyledButtonContainer>
          {hlsURI && (
            <MediaStreamer
              src={hlsURI}
              autoplay={autoplay}
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
        </Box>
      </PlayerContainer>
    </Box>
  )
}
