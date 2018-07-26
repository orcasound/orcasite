import React, {Component} from 'react'

import 'videojs-contrib-hls'
import videojs from 'video.js'

export default class MediaStreamer extends Component {
  componentDidMount() {

    this.player = videojs(this.videoNode, {
      sources: [{
        src: this.props.src,
        type: 'application/x-mpegurl'
      }]
    })
    this.player = videojs(this.videoNode, {
      hls: {
        liveSyncDurationCount: 12, //segments
      },
      sources: [{
        src: this.props.src,
        type: 'application/x-mpegurl'
      }]
    })

    this.player.ready(() => {
      this.props.onReady(this.controls)
      this.player.play()
    })

    this.player.on('playing', e => {
      this.props.onPlaying()
    })

    this.player.on('pause', () => {
      this.props.onPaused()
    })

    this.player.on('waiting', () => {
      this.props.onLoading()
    })
  }

  componentWillUnmount() {
    if (this.player) {
      this.player.dispose()
    }
  }

  play = () => {
    if (this.player) {
      this.player.play()
    }
  }

  pause = () => {
    if (this.player) {
      this.player.pause()
    }
  }

  playPause = () => {
    if (this.player) {
      this.player.paused() ? this.play() : this.pause()
    }
  }

  controls = {play: this.play, pause: this.pause, playPause: this.playPause}

  render() {
    const {src} = this.props

    return (
      <video ref={node => {this.videoNode = node}} className="video-js" />
    )
  }
}
