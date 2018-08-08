import React, {Component} from 'react'
import {Mutation} from 'react-apollo'
import {SUBMIT_DETECTION} from 'mutations/detections'

import {feedType} from 'types/feedType'
import {string, func, bool} from 'prop-types'

import Loader from 'components/Loader'

import 'styles/detect_button.scss'

export default class DetectButton extends Component {
  static propTypes = {
    isPlaying: bool.isRequired,
    feed: feedType.isRequired,
    timestamp: string.isRequired,
    getPlayerTime: func.isRequired,
  }

  static = {
    showReady: true,
    // showLoading
  }

  whale = () => '🐳'
  boat = () => '🚤'
  onDetect = submitDetection => {
    // Get current player time
    const {
      feed: {id: feedId},
      timestamp: playlistTimestamp,
      isPlaying,
      getPlayerTime,
    } = this.props
    const playerOffset = getPlayerTime()
    if (feedId && playlistTimestamp && playerOffset && isPlaying) {
      submitDetection({variables: {feedId, playlistTimestamp, playerOffset}})
    }
  }
  render() {
    return (
      <Mutation mutation={SUBMIT_DETECTION}>
        {(submitDetection, {loading, error, data}) => {
          const {isPlaying} = this.props
          return (
            <div
              className={`detect-button d-flex justify-content-center align-items-center border-left border-dark ${
                this.props.classNames
              } ${isPlaying ? '' : 'disabled'}`}
              onClick={() => {
                this.onDetect(submitDetection)
              }}>
              {loading && <Loader />}
              {!loading && !error && (
                <div className="text-nowrap">
                  {this.whale()}
                  {this.boat()} Activity
                </div>
              )}
              {error && (
                <div className="text-nowrap">
                  Error!
                </div>
              )}
            </div>
          )
        }}
      </Mutation>
    )
  }
}
