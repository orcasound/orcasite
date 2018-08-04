import React, {Component} from 'react'
import {Mutation} from 'react-apollo'
import {SUBMIT_DETECTION} from 'queries/detections'

import {feedType} from 'types/feedType'
import {string, func} from 'prop-types'

import 'styles/detect_button.scss'

export default class DetectButton extends Component {
  static propTypes = {
    feed: feedType.isRequired,
    timestamp: string.isRequired,
    getPlayerTime: func.isRequired
  }

  whale = () => "🐳"

  onDetect = submitDetection => {
    // Get current player time
    const {feed, timestamp} = this.props
    console.log("Clicked detect. Feed: ", this.props.feed, " timestamp ", this.props.timestamp)
    console.log("Player time is", this.props.getPlayerTime())
    const playerTime = this.props.getPlayerTime()
    if (feed && timestamp && playerTime) {
      submitDetection({variables: {feedId: feed.id, playlistTimestamp: timestamp, time: playerTime}})
    }
  }

  render() {
    return (
      <Mutation mutation={SUBMIT_DETECTION}>
        {(submitDetection, {data}) => {
          return (
            <div className={`detect-button d-flex justify-content-center align-items-center border-left border-dark ${this.props.classNames}`} onClick={() => { this.onDetect(submitDetection)}}>
              <div className="text-nowrap">
                {this.whale()} Activity!
              </div>
            </div>
          )
        }}
      </Mutation>
    )
  }
}
