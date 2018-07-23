import React, {Component} from 'react'
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faPlay, faPause, faSpinner} from '@fortawesome/free-solid-svg-icons'

import MediaStreamer from './MediaStreamer'

import 'styles/audio_player.scss'

export default class AudioPlayer extends Component {
  state = {
    timestamp: '',
    isLoading: true,
    isPlaying: false,
    play: () => {},
    pause: () => {},
    playPause: () => {},
  }

  playIconOpts = ({isLoading, isPlaying}) => {
    if (isLoading) return {icon: faSpinner, pulse: true}
    if (isPlaying) return {icon: faPause}
    return {icon: faPlay}
  }

  getHlsUri = (timestamp, feed) =>
    `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/${feed}/hls/${timestamp}/live.m3u8`

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

  componentDidMount() {
    this.fetchTimestamp(this.props.currentFeed)
    setInterval(() => this.fetchTimestamp(this.props.currentFeed), 10000)
  }

  setControls = controls => this.setState({...controls})

  render() {
    const {currentFeed} = this.props
    const {hlsURI, playPause} = this.state
    return (
      <div className="audio-player">
        <FontAwesomeIcon
          size="3x"
          {...this.playIconOpts(this.state)}
          className="mr-3"
          onClick={playPause}
        />
        Current feed: {currentFeed}
        {hlsURI && (
          <MediaStreamer src={hlsURI}
            onReady={this.setControls}
            onLoading={() => this.setState({isLoading: true})}
            onPlaying={() => this.setState({isLoading: false, isPlaying: true})}
            onPaused={() => this.setState({isLoading: false, isPlaying: false})}
          />
        )}
      </div>
    )
  }
}
