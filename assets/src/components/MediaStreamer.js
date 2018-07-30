import React, {Component} from 'react'

import videojs from 'video.js'

export default class MediaStreamer extends Component {
  componentDidMount() {

    const MAX_LATENCY = 300;
    const INITIAL_REWIND_AMOUNT = 90;
    const RETRY_REWIND_AMOUNT = 30;

    this.player = videojs(this.videoNode, {
      sources: [{
        src: this.props.src,
        type: 'application/x-mpegurl'
      }]
    })

    this.player.ready(() => {
      this.props.onReady(this.controls)
      // this.player.play()
      this.player.tech().one('progress', (e) => {
        // TODO: This seems to break if the first segment loads with no errors
        // Diabled for now
        //this.seekToLive(INITIAL_REWIND_AMOUNT)
      })
      this.player.tech().on('retryplaylist', (e) => {
        if (this.getLatency() < MAX_LATENCY) {
          this.rewind(RETRY_REWIND_AMOUNT)
        }
      })
      this.latencyUpdateInterval = setInterval(() => {
        this.props.onLatencyUpdate(this.getLatency(), this.player.currentTime())
      }, 1000);

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
    clearInterval(this.latencyUpdateInterval);
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

  seekToLive = (secondsFromLive = 30) => {
    if (this.player) {
      this.player.currentTime(this.player.seekable().end(0) - secondsFromLive)
    }
  }

  rewind = (seconds) => {
    if (this.player) {
      this.player.currentTime(this.player.currentTime() - seconds)
    }
  }

  getLatency = () => {
    if (this.player) {
      if (this.player.seekable().length > 0)
      {
        return this.player.seekable().end(0) - this.player.currentTime()
      }
      else return 0
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
