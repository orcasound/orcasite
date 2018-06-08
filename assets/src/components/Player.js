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
        <div className="container">
          <div className="row">
            <div className="col-sm">
              <h1>Orcasound App Beta</h1>
            </div>
          </div>
          <div className="row">
            <div className="col-sm">
              Please help us test playback performance on as many combinations of
              devices, operating systems, and browsers as possible.
            </div>
          </div>
          <div className="row">
            <div className="col-sm">
              <a href=''>Provide your feedback via this Google form</a>.
            </div>
          </div>
          <div className="row justify-content-lg-center mt-4">
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
          <div className="debug-info row p-6 alert alert-primary">
            <div className="col-sm">
              <div className="row">
                <div className="col-sm">
                  <h6>Debug links (for developers)</h6>
                </div>
              </div>
              <div><small>{this.state.timestamp}</small></div>
              <div><small><a className="alert-link" href={hlsUri}>{hlsUri}</a></small></div>
              <div><small><a className="alert-link" href={awsConsoleUri}>{awsConsoleUri}</a></small></div>
            </div>
          </div>
          
        </div>
      </div>
    )
  }
}
