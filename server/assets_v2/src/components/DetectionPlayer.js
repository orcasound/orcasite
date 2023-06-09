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

const SliderTime = styled.div`
  width: 100%;
  display: flex;
  flex-direction: column;
  align-items: stretch;
`

const TimeDisplay = styled.div`
  width: 100%;
  display: flex;
  justify-content: space-between;
`

export default class DetectionPlayer extends Component {
  static propTypes = {
    feed: object.isRequired,
    timestamp: number.isRequired,
    startOffset: number.isRequired,
    endOffset: number.isRequired
  }

  constructor(props) {
    super(props)

    this.state = {
      playerTime: 0,
      isPlaying: false,
      wasPlaying: false,
      duration: +props.endOffset - +props.startOffset
    }
  }

  playIconOpts = ({ isLoading, isPlaying }) => {
    if (isLoading) return { icon: faSpinner, pulse: true }
    if (isPlaying) return { icon: faPause }
    return { icon: faPlay }
  }

  setControls = controls => this.setState({ isLoading: false, ...controls })

  playerTimeToDisplayTime = playerTime =>
    Number(playerTime) - Number(this.props.startOffset)

  onSliderChange = (e, v) => {
    this.setState(
      prevState => ({
        playerTime: v,
        wasPlaying: prevState.isPlaying || prevState.wasPlaying
      }),
      () => this.state.pause()
    )
  }

  onSliderChangeCommitted = (e, v) => {
    this.state.setPlayerTime(v)
    if (this.state.wasPlaying) {
      this.setState({ wasPlaying: false }, this.state.play)
    }
  }

  formattedSeconds = seconds => {
    const mm = Math.floor(seconds / 60)
    const ss = seconds % 60
    return `${Number(mm)
      .toString()
      .padStart(2, "0")}:${ss.toFixed(0).padStart(2, "0")}`
  }

  render() {
    const {
      feed: { nodeName },
      timestamp,
      startOffset,
      endOffset,
      marks
    } = this.props
    return (
      <Player>
        <FontAwesomeIcon
          size="3x"
          {...this.playIconOpts(this.state)}
          className={classNames("m-3", { clickable: !this.state.isLoading })}
          onClick={this.state.playPause}
        />
        <SliderTime>
          <Slider
            step={0.1}
            max={this.state.duration}
            value={this.state.playerTime}
            marks={marks}
            onChange={this.onSliderChange}
            onChangeCommitted={this.onSliderChangeCommitted}
          />
          <TimeDisplay>
            <div>{this.formattedSeconds(this.state.playerTime.toFixed(0))}</div>
            <div>{this.formattedSeconds(this.state.duration.toFixed(0))}</div>
          </TimeDisplay>
        </SliderTime>
        <Hidden>
          <MediaStreamer
            src={feedSrc(nodeName, timestamp)}
            startOffset={startOffset}
            endOffset={endOffset}
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
