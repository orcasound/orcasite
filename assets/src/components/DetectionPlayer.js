import React, { Component } from "react"
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome"
import { faPlay, faPause, faSpinner } from "@fortawesome/free-solid-svg-icons"
import Slider from "@material-ui/core/Slider"
import { object, number } from "prop-types"
import classNames from "classnames"
import styled from "styled-components"

import { feedSrc } from "utils/feedStorage"
import MediaStreamer from "components/MediaStreamer"

import "styles/player.scss"

const Hidden = styled.div`
  display: none;
`

const Player = styled.div`
  display: flex;
  align-items: center;
`

export default class DetectionPlayer extends Component {
  static propTypes = {
    feed: object.isRequired,
    timestamp: number.isRequired,
    startOffset: number.isRequired,
    endOffset: number
  }

  constructor(props) {
    super(props)

    this.state = {
      playerTime: props.startOffset,
      endOffset: props.endOffset
    }
  }

  playIconOpts = ({ isLoading, isPlaying }) => {
    if (isLoading) return { icon: faSpinner, pulse: true }
    if (isPlaying) return { icon: faPause }
    return { icon: faPlay }
  }

  setControls = controls =>
    this.setState({ isLoading: false, ...controls }, () =>
      this.state.setPlayerTime(this.props.startOffset)
    )

  playerTimeToDisplayTime = playerTime =>
    Number(playerTime) - Number(this.props.startOffset)

  render() {
    const {
      feed: { nodeName },
      timestamp,
      startOffset
    } = this.props
    return (
      <Player>
        <FontAwesomeIcon
          size="3x"
          {...this.playIconOpts(this.state)}
          className={classNames("m-3", { clickable: !this.state.isLoading })}
          onClick={this.state.playPause}
        />
        <div>
          {this.playerTimeToDisplayTime(this.state.playerTime)}{" "}
          <Slider onChange={console.log} value={3} />
          {this.playerTimeToDisplayTime(this.props.endOffset)}
        </div>
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
            onTimeUpdate={playerTime => this.setState({ playerTime })}
          />
        </Hidden>
      </Player>
    )
  }
}
