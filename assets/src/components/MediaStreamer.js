import React, { Component } from "react"

import { bool, string, func, number } from "prop-types"

import videojs from "video.js"
import "videojs-offset"

export default class MediaStreamer extends Component {
  static propTypes = {
    src: string.isRequired,
    autoplay: bool,

    startOffset: number,
    endOffset: number,

    onReady: func,
    onPlaying: func,
    onPaused: func,
    onTimeUpdate: func,
    onLatencyUpdate: func
  }

  componentDidMount() {
    this.setupPlayer()
  }

  componentWillUnmount() {
    clearInterval(this.latencyUpdateInterval)
    this.pause()
    if (this.player) {
      this.player.dispose()
    }
  }

  setupPlayer = () => {
    clearInterval(this.latencyUpdateInterval)
    if (this.player) {
      this.player.dispose()
    }
    const MAX_LATENCY = 300
    const INITIAL_REWIND_AMOUNT = 90
    const RETRY_REWIND_AMOUNT = 30

    var options = {
      hls: {
        overrideNative: true
      }
    }

    this.player = videojs(this.audioNode, {
      autoplay: this.props.autoplay,
      flash: options,
      html5: options,
      sources: [
        {
          src: this.props.src,
          type: "application/x-mpegurl"
        }
      ]
    })

    if (this.props.startOffset && this.props.endOffset) {
      this.player.offset({
        start: this.props.startOffset,
        end: this.props.endOffset,
        restart_beginning: true
      })
    }

    this.player.ready(() => {
      this.props.onReady && this.props.onReady(this.controls)
      this.player.tech().on("retryplaylist", e => {
        if (this.getLatency() < MAX_LATENCY) {
          this.rewind(RETRY_REWIND_AMOUNT)
        }
      })
      this.latencyUpdateInterval = setInterval(() => {
        this.props.onLatencyUpdate &&
          this.props.onLatencyUpdate(
            this.getLatency(),
            this.player.currentTime()
          )
      }, 1000)
    })

    this.player.on("playing", () => {
      this.props.onPlaying && this.props.onPlaying()
    })

    this.player.on("pause", () => {
      this.props.onPaused && this.props.onPaused()
    })

    this.player.on("waiting", () => {
      this.props.onLoading && this.props.onLoading()
    })

    this.player.on("timeupdate", () => {
      this.props.onTimeUpdate &&
        this.props.onTimeUpdate(this.player.currentTime())
    })
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

  getPlayerTime = () => {
    if (this.player) {
      return this.player.currentTime()
    }
  }

  setPlayerTime = time => {
    if (this.player) this.player.currentTime(time)
  }

  seekToLive = (secondsFromLive = 30) => {
    if (this.player && this.player.readyState() > 0) {
      this.player.currentTime(this.player.seekable().end(0) - secondsFromLive)
    }
  }

  rewind = seconds => {
    if (this.player) {
      this.player.currentTime(this.player.currentTime() - seconds)
    }
  }

  getLatency = () => {
    if (this.player) {
      if (this.player.seekable().length > 0) {
        return this.player.seekable().end(0) - this.player.currentTime()
      } else return 0
    }
  }

  setVolume = volume => {
    if (this.player) {
      this.player.volume((volume/100))
    }
  }

  controls = {
    play: this.play,
    pause: this.pause,
    playPause: this.playPause,
    getPlayerTime: this.getPlayerTime,
    setPlayerTime: this.setPlayerTime,
    setVolume: this.setVolume
  }

  render() {
    return (
      <audio
        ref={node => {
          this.audioNode = node
        }}
        className="video-js"
        playsInline
      />
    )
  }
}
