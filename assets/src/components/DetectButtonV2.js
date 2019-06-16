import React, { Component } from 'react'
import { Mutation, renderToStringWithData } from 'react-apollo'
import { SUBMIT_DETECTION } from '../mutations/detections'

import { feedType } from '../types/feedType'
import { string, func, bool } from 'prop-types'

import Button from "@material-ui/core/Button"
import styled from "styled-components"

const StyledActivityButton = styled(Button)`
  background: #007166; 
  color: #ffffff;
  border-radius: 33px; 
  font-size: .875rem;
  letter-spacing: 0.5px;
  z-index: 10;
`

class DetectButtonV2 extends Component {
  static propTypes = {
    isPlaying: bool.isRequired,
    feed: feedType.isRequired,
    timestamp: string.isRequired,
    getPlayerTime: func.isRequired,
  }

  constructor(props) {
    super(props)

    this.state = {
      showDefault: true,
      showSubmitting: false,
      showError: false,
      showSuccess: false,
      showLockout: false,

      lockoutInitial: 0,
      lockoutProgress: 1,
      lockoutDefault: 10,
      lockoutUpdateInterval: 100,
      messageTimeout: 2000,
    }
  }

  onDetect = submitDetection => {
    const {
      feed: { id: feedId },
      timestamp: playlistTimestamp,
      isPlaying,
      getPlayerTime,
    } = this.props

    const { showDefault } = this.state

    const playerOffset = getPlayerTime()
    if (
      feedId &&
      playlistTimestamp &&
      playerOffset &&
      showDefault &&
      isPlaying
    ) {
      this.setState({ showSubmitting: true, showDefault: false }, () => {
        submitDetection({
          variables: { feedId, playlistTimestamp, playerOffset },
        })
      })
    }
  }

  onSuccess = ({ submitDetection: { lockoutInitial, lockoutRemaining } }) => {
    const lockoutStart = lockoutInitial || this.state.lockoutDefault
    const lockoutCurrent = lockoutRemaining || lockoutStart

    this.processResponse({ showSuccess: true }, lockoutStart, lockoutCurrent)
  }

  onError = ({ graphQLErrors }) => {
    const {
      details: {
        lockout_initial: lockoutInitial = this.state.lockoutDefault,
        lockout_remaining: lockoutRemaining = this.state.lockoutDefault,
      },
    } = graphQLErrors.find(err => err.message === 'lockout' || { details: {} })

    this.processResponse({ showError: true }, lockoutInitial, lockoutRemaining)
  }

  processResponse = (newState, lockoutStart, lockoutCurrent) => {
    this.setState({
      ...newState,
      showSubmitting: false,
      showLockout: true,
      lockoutInitial: lockoutStart,
      lockoutProgress: (lockoutStart - lockoutCurrent) / 100,
      lockoutIntervalId: setInterval(() => {
        this.setLockoutProgress()
      }, this.state.lockoutUpdateInterval),
      messageTimeoutId: setTimeout(() => {
        this.setState({ showSuccess: false, showError: false })
      }, this.state.messageTimeout),
    })
  }

  setLockoutProgress() {
    const {
      showLockout,
      lockoutInitial,
      lockoutProgress,
      lockoutUpdateInterval,
      lockoutIntervalId,
    } = this.state

    const newLockoutProgress = Math.min(
      (lockoutInitial * lockoutProgress + lockoutUpdateInterval / 1000) / lockoutInitial,
      1,
    )
    if (newLockoutProgress >= 1) {
      clearInterval(this.state.lockoutIntervalId)
    }
    this.setState({
      lockoutProgress: newLockoutProgress,
      showLockout: newLockoutProgress < 1,
      showDefault: newLockoutProgress >= 1,
    })
  }

  componentWillUnmount() {
    // close any message intervals
    clearTimeout(this.state, messageTimeoutId)
    clearInterval(this.state.lockoutIntervalId)
  }

  render() {
    const {
      showDefault,
      showSubmitting,
      showError,
      showSuccess,
      showLockout,
      lockoutProgress
    } = this.state

    return (
      <Mutation
        mutation={SUBMIT_DETECTION}
        onError={this.onError}
        onCompleted={this.onSuccess}>
        {(submitDetection, { data }) => {
          return (
            <StyledActivityButton
              aria-label="I hear something interesting">
              I hear something interesting
          </StyledActivityButton>
          )
        }}
      </Mutation>
    )
  }
}

export default DetectButtonV2
