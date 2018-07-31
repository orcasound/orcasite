import React, {Component} from 'react'
import {Mutation} from 'react-apollo'
import {SUBMIT_DETECTION} from 'queries/detections'

import {feedType} from 'types/feedType'
import {string, func} from 'prop-types'

export default class DetectButton extends Component {
  static propTypes = {
    feed: feedType.isRequired,
    timestamp: string.isRequired,
    getPlayerTime: func.isRequired
  }

  whale = () => "🐳"
  sound = () => "🎶"

  onDetect = submitDetection => {
    // Get current player time
    console.log("Clicked detect. Feed: ", this.props.feed, " timestamp ", this.props.timestamp)
    console.log("Player time is", this.props.getPlayerTime())
  }

  render() {
    return (
      <Mutation mutation={SUBMIT_DETECTION}>
        {(submitDetection, {data}) => {
          return <div className="detect-button" onClick={() => { this.onDetect(submitDetection)}}>{this.whale()} Activity!  {this.sound()}</div>
        }}
      </Mutation>
    )
  }
}
