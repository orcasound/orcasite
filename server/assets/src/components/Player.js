import React, { Component } from "react"
import { Fab, Box, Grid, Slider } from "@material-ui/core"
import { PlayArrow, Pause, VolumeUp, VolumeDown } from "@material-ui/icons"



import MediaStreamer from "./MediaStreamer"
import DetectionDialog from "./DetectionDialog"

import { feedType } from "../types/feedType"
import { storeCurrentFeed, getCurrentFeed } from "../utils/feedStorage"

import styled from "styled-components"
import analyticsEvents from "../utils/analyticsEvents"

const PlayerContainer = styled.div`
  .video-js {
    display: none;
  }
`

const StyledButtonContainer = styled.div`
  background: ${props => (props.active ? "#007166" : "transparent")};
  box-shadow: ${props => (props.active ? "0 0.125rem 0.25rem 0" : "none")};
  border-radius: 2.0625rem;
  min-width: 21.0625rem;
  max-width: 28.125rem;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: flex-start;
`
const VolumeContainer = styled.div`
  visibility: ${props => (props.active ? "visible" : "hidden")};
  min-width: 21.0625rem;
  max-width: 28.125rem;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: flex-start;
`

class Player extends Component {
  static propTypes = {
    feed: feedType
  }

  constructor(props) {
    super(props)

    const currentFeed = this.props.currentFeed || getCurrentFeed() || {}
    storeCurrentFeed(currentFeed)

    this.state = {
      currentFeed,
      timestamp: "",
      isLoading: true,
      isPlaying: false,
      intervalId: null,
      debugInfo: {
        playerTime: 0,
        latencyHistory: [0]
      },
      play: () => {},
      pause: () => {},
      playPause: () => {},
      getPlayerTime: () => {},
      setVolume: () => {},
    }
  }

  isEmpty = object => Object.keys(object).length === 0

  startTimestampFetcher = () => {
    var { currentFeed, intervalId } = this.state

    this.clearInterval()
    if (currentFeed && Object.keys(currentFeed).length > 0) {
      this.fetchTimestamp(currentFeed.nodeName)
      intervalId = setInterval(
        () => this.fetchTimestamp(currentFeed.nodeName),
        10000
      )

      this.setState({ intervalId })
    }
  }

  componentDidMount() {
    this.startTimestampFetcher()
  }

  componentWillUnmount() {
    this.clearInterval()
  }

  clearInterval = () => {
    const { intervalId, currentXhr } = this.state
    if (currentXhr) currentXhr.abort()
    if (intervalId) clearInterval(intervalId)
  }

  debugInfo = (hlsURI, awsConsoleUri) => (
    console.log("Stream Latency = ", Math.round(this.getStreamLatency())),
    console.log("Total Latency = ", Math.round(this.getTotalLatency()))
  )

  getStreamLatency = () => {
    return this.state.debugInfo.latencyHistory[
      this.state.debugInfo.latencyHistory.length - 1
    ]
  }

  getTotalLatency = () => {
    return (
      Math.floor(Date.now() / 1000) -
      (+this.state.timestamp + this.startTimestampFetcher.debugInfo.playerTime)
    )
  }

  getHlsUri = (timestamp, feed, bucket) =>
    `https://s3-us-west-2.amazonaws.com/${bucket}/${feed}/hls/${timestamp}/live.m3u8`

  getAwsConsoleUri = (timestamp, nodeName, bucket) =>
    `https://s3.console.aws.amazon.com/s3/buckets/${bucket}/${nodeName}/hls/${timestamp}/`

  fetchTimestamp = feed => {
    const timestampURI = `https://s3-us-west-2.amazonaws.com/${ENV.S3_BUCKET}/${feed}/latest.txt`

    const xhr = new XMLHttpRequest()
    this.setState({ currentXhr: xhr })
    xhr.open("GET", timestampURI)
    xhr.onload = () => {
      if (xhr.status === 200) {
        const timestamp = xhr.responseText.trim()
        if (ENV.DEVELOPMENT) console.log("Latest timestamp: " + timestamp)
        if (timestamp != this.state.timestamp) {
          this.setState({
            timestamp: timestamp,
            hlsURI: this.getHlsUri(timestamp, feed, ENV.S3_BUCKET)
          })
          if (ENV.DEVELOPMENT)
            console.log(
              "New stream instance: " +
                this.getHlsUri(timestamp, feed, ENV.S3_BUCKET)
            )
        }
      }
    }
    xhr.send()
  }

  setControls = controls => this.setState({ isLoading: false, ...controls })


  render() {
    const {
      hlsURI,
      playPause,
      currentFeed,
      timestamp,
      isLoading,
      isPlaying,
      getPlayerTime,
      setVolume
    } = this.state

    const awsConsoleUri = this.getAwsConsoleUri(
      timestamp,
      currentFeed.nodeName,
      ENV.S3_BUCKET
    )

    const handleVolumeChange = (e, newValue) => {
      setVolume(newValue);
    };

    if (currentFeed && Object.keys(currentFeed).length !== 0) {
      return (
        <PlayerContainer>
          <Box
            mt={{ xs: -3.5, sm: -3.5 }}
            ml={{ xs: 3, sm: 9, md: 12, lg: 20 }}
          >
            <StyledButtonContainer active={isPlaying}>
              <Fab color="secondary" onClick={playPause}>
                {!isPlaying && (
                  <PlayArrow
                    className="icon"
                    fontSize="large"
                    onClick={() =>
                      analyticsEvents.stream.started(currentFeed.slug)
                    }
                  />
                )}
                {isPlaying && (
                  <Pause
                    className="icon"
                    fontSize="large"
                    onClick={() =>
                      analyticsEvents.stream.paused(currentFeed.slug)
                    }
                  />
                )}
              </Fab>

              {isPlaying && (
                <DetectionDialog
                  isPlaying={isPlaying}
                  feed={currentFeed}
                  timestamp={timestamp}
                  getPlayerTime={getPlayerTime}
                  listenerCount={this.props.listenerCount}
                />
              )}
            </StyledButtonContainer>
            {hlsURI && (
              <MediaStreamer
                src={hlsURI}
                autoplay={this.props.autoplay}
                onReady={this.setControls}
                onLoading={() => this.setState({ isLoading: true })}
                onPlaying={() =>
                  this.setState({ isLoading: false, isPlaying: true })
                }
                onPaused={() =>
                  this.setState({ isLoading: false, isPlaying: false })
                }
                onLatencyUpdate={(newestLatency, playerTime) =>
                  this.setState({
                    debugInfo: {
                      playerTime: playerTime,
                      latencyHistory: this.state.debugInfo.latencyHistory.concat(
                        newestLatency
                      )
                    }
                  })
                }
              />
            )}
            <VolumeContainer active={isPlaying}>
              <Grid container spacing={2}>
                <Grid item>
                  <VolumeDown />
                </Grid>
                <Grid item xs>
                  <Slider defaultValue={100} onChange={handleVolumeChange} aria-labelledby="continuous-slider" />
                </Grid>
                <Grid item>
                  <VolumeUp />
                </Grid>
              </Grid>
            </VolumeContainer>  
          </Box>
        </PlayerContainer>
      )
    }
  }
}

export default Player
