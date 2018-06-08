import React, { Component } from 'react'

import MediaElement from './MediaElement';
import 'styles/player.scss'

export default class Player extends Component {

  state = {
    timestamp: ''
  }

  getHlsUri(timestamp, nodeName) {
    return `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/${nodeName}/hls/${timestamp}/live.m3u8`
  }

  getAwsConsoleUri(timestamp, nodeName) {
    return `https://s3.console.aws.amazon.com/s3/buckets/dev-streaming-orcasound-net/${nodeName}/hls/${timestamp}/`
  }

  fetchTimestamp = () => {
    const nodeName = this.props.nodeName
    const timestampURI = `https://s3-us-west-2.amazonaws.com/dev-streaming-orcasound-net/${nodeName}/latest.txt`

    var xhr = new XMLHttpRequest()
    xhr.open('GET', timestampURI)
    xhr.onload = () => {
      if (xhr.status === 200){
        var timestamp = xhr.responseText.trim()
        console.log("Latest timestamp: " + timestamp)
        if (timestamp != this.state.timestamp) {
          this.setState({timestamp: timestamp})
          console.log("New stream instance: " + this.getHlsUri(timestamp, nodeName))
        }
      }
    }
    xhr.send()
  }

  componentDidMount() {
    this.fetchTimestamp()
    setInterval(this.fetchTimestamp, 10000)
  }

  render() {  
    var timestamp = this.state.timestamp
    const nodeName = this.props.nodeName
    var hlsUri = this.getHlsUri(timestamp, nodeName)
    var awsConsoleUri = this.getAwsConsoleUri(timestamp, nodeName)

    const
      sources = [
        {src: hlsUri, type: 'application/vnd.apple.mpegurl'}
      ],
      config = {
        hls: {
          debug: true
        }},
      tracks = {}
    ;

    return (
      <div className="player">
        <h1>{this.state.timestamp}</h1>
        <h3><a href={hlsUri}>{hlsUri}</a></h3>
        <h3><a href={awsConsoleUri}>{awsConsoleUri}</a></h3>
        <div className="d-flex justify-content-center mt-4">
          <MediaElement
           key={hlsUri}
           id="player1"
           mediaType="audio"
           preload="auto"
           controls
           width="640"
           height="360"
           poster=""
           autoplay
           sources={JSON.stringify(sources)}
           options={JSON.stringify(config)}
           tracks={JSON.stringify(tracks)}
          />
        </div>
      </div>
    )
  }
}
