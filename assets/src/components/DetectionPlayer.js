import React, { Component } from "react"
import { feedSrc } from "utils/feedStorage"
import MediaStreamer from "components/MediaStreamer"

import { FontAwesomeIcon } from "@fortawesome/react-fontawesome"
import { faPlay, faPause, faSpinner } from "@fortawesome/free-solid-svg-icons"

import { object, number } from "prop-types"
import classNames from "classnames"
import styled from "styled-components"

import "styles/player.scss"

const Hidden = styled.div`
  display: none;
`

export default class DetectionPlayer extends Component {
  static propTypes = {
    feed: object.isRequired,
    timestamp: number,
    offset: number
  }

  state = {}

  playIconOpts = ({ isLoading, isPlaying }) => {
    if (isLoading) return { icon: faSpinner, pulse: true }
    if (isPlaying) return { icon: faPause }
    return { icon: faPlay }
  }

  setControls = controls => this.setState({ isLoading: false, ...controls })

  render() {
    const {
      feed: { nodeName },
      timestamp,
      offset
    } = this.props
    return (
      <div>
        <FontAwesomeIcon
          size="3x"
          {...this.playIconOpts(this.state)}
          className={classNames("m-3", { clickable: !this.state.isLoading })}
          onClick={this.state.playPause}
        />
        <Hidden>
          <MediaStreamer
            src={feedSrc(nodeName, timestamp)}
            onReady={this.setControls}
            onLoading={() => this.setState({ isLoading: true })}
            onPlaying={() =>
              this.setState({ isLoading: false, isPlaying: true })
            }
            onPaused={() =>
              this.setState({ isLoading: false, isPlaying: false })
            }
            style="display: none"
          />
        </Hidden>
      </div>
    )
  }
}
