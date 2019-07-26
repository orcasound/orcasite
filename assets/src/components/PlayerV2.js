import React, { Component } from "react"

import { PlayArrow, Pause } from "@material-ui/icons"
import Fab from "@material-ui/core/Fab"

import MediaStreamerV2 from "./MediaStreamerV2"
import DetectionDialogV2 from "./DetectionDialogV2"

import { feedType } from "../types/feedType"
import { storeCurrentFeed, getCurrentFeed } from "../utils/feedStorage"

import styled from "styled-components"

const PlayerContainer = styled.div`
  margin: -2rem 1rem 1rem 1rem;
  z-index: 10;

  .video-js {
    display: none;
  }
`

const StyledButtonContainer = styled.div`
  background: ${props => (props.active ? "#007166" : "transparent")};
  box-shadow: ${props => (props.active ? "0 2px 4px 0" : "none")};
  border-radius: 33px;
  min-width: 337px;
  max-width: 450px;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: flex-start;
`

class PlayerV2 extends Component {
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
      getPLayerTime: () => {}
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
    const timestampURI = `https://s3-us-west-2.amazonaws.com/${
      ENV.S3_BUCKET
    }/${feed}/latest.txt`

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
      getPlayerTime
    } = this.state

    const awsConsoleUri = this.getAwsConsoleUri(
      timestamp,
      currentFeed.nodeName,
      ENV.S3_BUCKET
    )

    if (currentFeed && Object.keys(currentFeed).length !== 0) {
      return (
        <PlayerContainer>
          <StyledButtonContainer active={isPlaying}>
            <Fab color="secondary" onClick={playPause}>
              {!isPlaying && <PlayArrow className="icon" fontSize="large" />}
              {isPlaying && <Pause className="icon" fontSize="large" />}
            </Fab>

            {isPlaying && (
              <DetectionDialogV2
                isPlaying={isPlaying}
                feed={currentFeed}
                timestamp={timestamp}
                getPlayerTime={getPlayerTime}
              />
            )}
          </StyledButtonContainer>
          {hlsURI && (
            <MediaStreamerV2
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
        </PlayerContainer>
      )
    }
  }
}

export default PlayerV2
