import React, { Component } from "react"
import { Link } from "react-router-dom"

import Button from "@material-ui/core/Button"
import { PlayArrow, Pause } from "@material-ui/icons"

import MediaStreamerV2 from "./MediaStreamerV2"

import FeedPresence from "./FeedPresence"

import { feedType } from "../types/feedType"
import { storeCurrentFeed, getCurrentFeed } from "../utils/feedStorage"

import styled from "styled-components"

const StyledMediaContainer = styled.div`
  .video-js {
    display: none;
    }
`

const StyledButtonContainer = styled.div`
  background: #007166; 
  border-radius: 33px;
  box-shadow: 0 2px 4px 0;
  width: 337px;
  display: flex;
`

const StyledMuiButton = styled(Button)`
  background: #009688;
  border-radius: 33px;
  color: #ffffff;
  .icon {
    color: #ffffff;
    font-size: 3rem;
  }
`

const StyledActivityButton = styled(Button)`
  background: #007166; 
  color: #ffffff;

  border-radius: 33px;
  box-shadow: 0 2px 4px 0; 
  font-size: .875rem;
  letter-spacing: 0.5px;
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
      timestamp: '',
      isLoading: true,
      isPlaying: false,
      intervalId: null,
      debugInfo: {
        playerTime: 0,
        latencyHistory: [0],
      },
      play: () => { },
      pause: () => { },
      playPause: () => { },
      getPLayerTime: () => { },
      playButtonClicked: true,
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
        10000,
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
    xhr.open('GET', timestampURI)
    xhr.onload = () => {
      if (xhr.status === 200) {
        const timestamp = xhr.responseText.trim()
        if (ENV.DEVELOPMENT) console.log('Latest timestamp: ' + timestamp)
        if (timestamp != this.state.timestamp) {
          this.setState({
            timestamp: timestamp,
            hlsURI: this.getHlsUri(timestamp, feed, ENV.S3_BUCKET),
          })
          if (ENV.DEVELOPMENT)
            console.log(
              "New stream instance: " +
              this.getHlsUri(timestamp, feed, ENV.S3_BUCKET),
            )
        }
      }
    }
    xhr.send()
  }

  setControls = controls => this.setState({ isLoading: false, ...controls })



  render() {
    console.log('playPause', this.state.playPause)
    const {
      hlsURI,
      playPause,
      currentFeed,
      timestamp,
      isLoading,
      isPlaying,
      getPlayerTime,
    } = this.state

    const awsConsoleUri = this.getAwsConsoleUri(
      timestamp,
      currentFeed.nodeName,
      ENV.S3_BUCKET,
    )

    if (currentFeed && Object.keys(currentFeed).length !== 0) {
      return (
        <div>
          <StyledButtonContainer>
            <StyledMuiButton aria-label="Play/Pause" onClick={playPause}>
              <PlayArrow className="icon" fontSize="large" />
              <Pause className="icon" fontSize="large" />
            </StyledMuiButton>
            <StyledActivityButton aria-label="I hear something interesting">
              I hear something interesting
          </StyledActivityButton>
          </StyledButtonContainer>
          {
            hlsURI && (
              <StyledMediaContainer>
                <MediaStreamerV2
                  src={hlsURI}
                  autoplay={this.props.autoplay}
                  onReady={this.setControls}
                  onLoading={() => this.setState({ isLoading: true })}
                  onPlaying={() =>
                    this.setState({ isLoading: false, isPlaying: false })
                  }
                  onPaused={() =>
                    this.setState({ isLoading: false, isPlaying: false })}
                  onLatencyUpdate={(newestLatency, playerTime) =>
                    this.setState({
                      debugInfo: {
                        playerTime: playerTime,
                        latencyHistory: this.state.debugInfo.latencyHistory.concat(
                          newestLatency,
                        ),
                      },
                    })
                  }
                />
              </StyledMediaContainer>
            )
          }
        </div >
      )
    }
  }
}

export default PlayerV2
