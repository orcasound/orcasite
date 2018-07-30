import React, {Component} from 'react'
import {Link} from 'react-router-dom'

import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faPlay, faPause, faSpinner} from '@fortawesome/free-solid-svg-icons'

import MediaStreamer from './MediaStreamer'

import {feedType} from 'types/feedType'
import {storeCurrentFeed, getCurrentFeed} from 'utils/feedStorage'

import 'styles/player.scss'

export default class Player extends Component {
  static propTypes = {
    feed: feedType,
  }

  constructor(props) {
    super(props)

    const currentFeed = this.props.currentFeed || getCurrentFeed() || {}

    this.state = {
      currentFeed,
      timestamp: '',
      isLoading: true,
      isPlaying: false,
      debugInfo: {
        playerTime: 0,
        latencyHistory: [0],
      },
      play: () => {},
      pause: () => {},
      playPause: () => {},
    }
  }

  isEmpty = object => Object.keys(object).length === 0

  componentDidMount() {
    this.fetchTimestamp(this.state.currentFeed.nodeName)
    setInterval(
      () => this.fetchTimestamp(this.state.currentFeed.nodeName),
      10000,
    )
  }

  componentDidUpdate(prevProps) {
    const {currentFeed} = this.props
    if (currentFeed !== prevProps.currentFeed) {
      storeCurrentFeed(currentFeed)
      this.setState({currentFeed})
    }
  }

  playIconOpts = ({isLoading, isPlaying}) => {
    if (isLoading) return {icon: faSpinner, pulse: true}
    if (isPlaying) return {icon: faPause}
    return {icon: faPlay}
  }

  debugInfo = (hlsUri, awsConsoleUri) => (
    <div className="ml-auto">
      <span className="mr-2" title="Stream Latency">
        {Math.round(this.getStreamLatency())}
      </span>
      <span className="mr-2" title="Total Latency">
        {Math.round(this.getTotalLatency())}
      </span>
      <a href={hlsUri} className="mx-2" target="_blank">
        HLS
      </a>
      <a href={awsConsoleUri} className="mx-2" target="_blank">
        AWS
      </a>
    </div>
  )

  getStreamLatency = () => {
    return this.state.debugInfo.latencyHistory[
      this.state.debugInfo.latencyHistory.length - 1
    ]
  }

  getTotalLatency = () => {
    return (
      Math.floor(Date.now() / 1000) -
      (+this.state.timestamp + this.state.debugInfo.playerTime)
    )
  }

  getHlsUri = (timestamp, feed) =>
    `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/${feed}/hls/${timestamp}/live.m3u8`

  getAwsConsoleUri = (timestamp, nodeName) =>
    `https://s3.console.aws.amazon.com/s3/buckets/dev-streaming-orcasound-net/${nodeName}/hls/${timestamp}/`

  fetchTimestamp = feed => {
    const timestampURI = `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/${feed}/latest.txt`

    const xhr = new XMLHttpRequest()
    xhr.open('GET', timestampURI)
    xhr.onload = () => {
      if (xhr.status === 200) {
        const timestamp = xhr.responseText.trim()
        if (ENV.development) console.log('Latest timestamp: ' + timestamp)
        if (timestamp != this.state.timestamp) {
          this.setState({
            timestamp: timestamp,
            hlsURI: this.getHlsUri(timestamp, feed),
          })
          if (ENV.development)
            console.log(
              'New stream instance: ' + this.getHlsUri(timestamp, feed),
            )
        }
      }
    }
    xhr.send()
  }

  setControls = controls => this.setState({...controls})

  render() {
    const {hlsURI, playPause, currentFeed} = this.state

    const awsConsoleUri = this.getAwsConsoleUri(
      this.state.timestamp,
      currentFeed.nodeName,
    )

    if (currentFeed && Object.keys(currentFeed).length !== 0) {
      return (
        <div className="audio-player text-light d-flex align-items-center justify-content-start">
          <FontAwesomeIcon
            size="3x"
            {...this.playIconOpts(this.state)}
            className="mr-3"
            onClick={playPause}
          />
          {currentFeed.slug && (
            <Link to={currentFeed.slug} className="text-light">
              {currentFeed.name}
            </Link>
          )}
          {hlsURI && (
            <MediaStreamer
              src={hlsURI}
              onReady={this.setControls}
              onLoading={() => this.setState({isLoading: true})}
              onPlaying={() =>
                this.setState({isLoading: false, isPlaying: true})
              }
              onPaused={() =>
                this.setState({isLoading: false, isPlaying: false})
              }
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
          )}
          {this.debugInfo(hlsURI, awsConsoleUri)}
        </div>
      )
    }

    return <div />
  }
}
