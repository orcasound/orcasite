import React, { Component } from 'react'

import Hls from 'hls.js'

import 'styles/player.scss'

export default class Player extends Component {

  state = {
    timestamp: ''
  }

  getHlsUri() {
    return `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/rpi_seattle/hls/${this.state.timestamp}/live.m3u8`
  }

  getAwsConsoleUri() {
    return `https://s3.console.aws.amazon.com/s3/buckets/dev-streaming-orcasound-net/rpi_seattle/hls/${this.state.timestamp}/`
  }

  reloadSource() {
    var hlsUri = this.getHlsUri()
    hls.loadSource(hlsUri);
      hls.attachMedia(audio);
      hls.config.liveSyncDurationCount = 5;
      hls.on(Hls.Events.MANIFEST_PARSED,function() {
        audio.play();
      });
  }

  fetchTimestamp = () => {
    const timestampURI = 'https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/rpi_seattle/latest.txt'

    var xhr = new XMLHttpRequest()
    xhr.open('GET', timestampURI)
    xhr.onload = () => {
      var timestamp = xhr.responseText.trim()
      console.log("Latest timestamp: " + timestamp)
      if (timestamp != this.state.timestamp) {
        this.setState({timestamp: timestamp})
        console.log("New stream instance: " + this.getHlsUri())
        this.reloadSource()
      }
    }
    xhr.send()
  }

  componentDidMount() {
    // Create a Player instance.
    var audio = document.getElementById('audio')

    var hls = new Hls()


    // Attach player to the window to make it easy to access in the JS console.
    window.hls = hls
    this.hls = hls

    this.fetchTimestamp()
    setInterval(this.fetchTimestamp, 10000)
  }

  render() {   
    var hlsUri = this.getHlsUri()
    var awsConsoleUri = this.getAwsConsoleUri()

    return (
      <div className="player">
        <h1>{this.state.timestamp}</h1>
        <h3><a href={hlsUri}>{hlsUri}</a></h3>
        <h3><a href={awsConsoleUri}>{awsConsoleUri}</a></h3>
        <audio id="audio"
             controls autoPlay>
        </audio>
      </div>
    )
  }
}
