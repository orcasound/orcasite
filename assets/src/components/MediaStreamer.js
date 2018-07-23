import React, {Component} from 'react'

import 'videojs-flash'
import 'videojs-contrib-hls'
import videojs from 'video.js'

// import 'video.js/dist/video-js.css'

export default class MediaStreamer extends Component {
  componentDidMount() {
    // instantiate Video.js
    this.player = videojs(this.videoNode)

    window.player = this.player
    window.videojs = videojs

    this.player.ready(() => {
      this.props.onReady(this.controls)
      this.player.play()
    })

    this.player.on('playing', (e) => {
      this.props.onPlaying()
    })

    this.player.on('pause', () => {
      this.props.onPaused()
    })

    this.player.on('waiting', () => {
      this.props.onLoading()
    })


  }

  // destroy player on unmount
  componentWillUnmount() {
    if (this.player) {
      this.player.dispose()
    }
  }

  play = () => {
    this.player.play()
  }

  pause = () => {
    this.player.pause()
  }

  playPause = () => {
    this.player.paused() ? this.play() : this.pause()
  }

  controls = {play: this.play, pause: this.pause, playPause: this.playPause}

  render() {
    // const { src } = this.props
    const src = ENV.example_playlist_uri

    return (
      <video ref={node => (this.videoNode = node)} className="video-js" autoPlay controls >
        <source src={src} type="application/x-mpegURL" />
      </video>
    )
  }
}
