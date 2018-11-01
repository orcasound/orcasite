import React, {Component} from 'react'
import {Link} from 'react-router-dom'

import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faPlay, faPause, faSpinner} from '@fortawesome/free-solid-svg-icons'

import MediaStreamer from './MediaStreamer'
import DetectButton from './DetectButton'
import FeedPresence from './FeedPresence'

import {feedType} from 'types/feedType'
import {storeCurrentFeed, getCurrentFeed} from 'utils/feedStorage'
import classNames from 'classnames'

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
      intervalId: null,
      debugInfo: {
        playerTime: 0,
        latencyHistory: [0],
      },
      play: () => {},
      pause: () => {},
      playPause: () => {},
      getPlayerTime: () => {},
    }
  }

  isEmpty = object => Object.keys(object).length === 0

  startTimestampFetcher = () => {
    var {currentFeed, intervalId} = this.state

    this.clearInterval()
    if (currentFeed && Object.keys(currentFeed).length > 0) {
      this.fetchTimestamp(currentFeed.nodeName)
      intervalId = setInterval(
        () => this.fetchTimestamp(currentFeed.nodeName),
        10000,
      )

      this.setState({intervalId})
    }
  }

  componentDidMount() {
    this.startTimestampFetcher()
  }

  componentWillUnmount() {
    this.clearInterval()
  }

  componentDidUpdate(prevProps) {
    const {currentFeed} = this.props
    if (currentFeed !== prevProps.currentFeed) {
      storeCurrentFeed(currentFeed)
      this.clearInterval()
      this.setState({currentFeed}, this.startTimestampFetcher)
    }
  }

  clearInterval = () => {
    const {intervalId, currentXhr} = this.state
    if (currentXhr) currentXhr.abort()
    if (intervalId) clearInterval(intervalId)
  }

  playIconOpts = ({isLoading, isPlaying}) => {
    if (isLoading) return {icon: faSpinner, pulse: true}
    if (isPlaying) return {icon: faPause}
    return {icon: faPlay}
  }

  debugInfo = (hlsUri, awsConsoleUri) => (
    <div className="">
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

  getHlsUri = (timestamp, feed, bucket) =>
    `https://s3-us-west-2.amazonaws.com/${bucket}/${feed}/hls/${timestamp}/live.m3u8`

  getAwsConsoleUri = (timestamp, nodeName, bucket) =>
    `https://s3.console.aws.amazon.com/s3/buckets/${bucket}/${nodeName}/hls/${timestamp}/`

  fetchTimestamp = feed => {
    const timestampURI = `https://s3-us-west-2.amazonaws.com/${
      ENV.S3_BUCKET
    }/${feed}/latest.txt`

    const xhr = new XMLHttpRequest()
    this.setState({currentXhr: xhr})
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
              'New stream instance: ' +
                this.getHlsUri(timestamp, feed, ENV.S3_BUCKET),
            )
        }
      }
    }
    xhr.send()
  }

  setControls = controls => this.setState({isLoading: false, ...controls})

  render() {
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
        <div className="player">
          <FontAwesomeIcon
            size="3x"
            {...this.playIconOpts(this.state)}
            className={classNames('m-3', {clickable: !isLoading})}
            onClick={playPause}
          />
          {currentFeed.slug && (
            <Link to={currentFeed.slug} className="text-light my-3">
              {currentFeed.name}
            </Link>
          )}
          {hlsURI && (
            <MediaStreamer
              src={hlsURI}
              autoplay={this.props.autoplay}
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
          <div className="ml-auto d-flex pr-3">
            {ENV.SHOW_PLAYER_DEBUG_INFO &&
              this.debugInfo(hlsURI, awsConsoleUri)}
            <FeedPresence feed={currentFeed} className="ml-2" />
          </div>
          {ENV.FEATURE_ACTIVITY_BUTTON && (
            <DetectButton
              isPlaying={isPlaying}
              feed={currentFeed}
              getPlayerTime={getPlayerTime}
              timestamp={timestamp}
            />
          )}
        </div>
      )
    }

    return <div className="player" />
  }
}
