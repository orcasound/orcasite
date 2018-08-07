import React, {Component} from 'react'
import {Mutation} from 'react-apollo'
import {SUBMIT_DETECTION} from 'mutations/detections'

import {feedType} from 'types/feedType'
import {string, func, bool} from 'prop-types'

import 'styles/detect_button.scss'

export default class DetectButton extends Component {
  static propTypes = {
    isPlaying: bool.isRequired,
    feed: feedType.isRequired,
    timestamp: string.isRequired,
    getPlayerTime: func.isRequired
  }

  whale = () => "🐳"

  onDetect = submitDetection => {
    // Get current player time
    const {feed: {id: feedId}, timestamp: playlistTimestamp, isPlaying, getPlayerTime} = this.props
    const playerOffset = getPlayerTime()
    if (feedId && playlistTimestamp && playerOffset && isPlaying) {
      submitDetection({variables: {feedId, playlistTimestamp, playerOffset}})
    }
  }

  render() {
    return (
      <Mutation mutation={SUBMIT_DETECTION}>
        {(submitDetection, {data}) => {
          const {isPlaying} = this.props
          return (
            <div className={`detect-button d-flex justify-content-center align-items-center border-left border-dark ${this.props.classNames} ${isPlaying ? '' : 'disabled'}`} onClick={() => { this.onDetect(submitDetection)}}>
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
