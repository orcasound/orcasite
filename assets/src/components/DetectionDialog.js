import React, { Component } from "react"

import { Mutation } from "react-apollo"
import { SUBMIT_DETECTION } from "../mutations/detections"

import { feedType } from "../types/feedType"
import { string, func, bool } from "prop-types"

import Button from "@material-ui/core/Button"
import TextField from "@material-ui/core/TextField"
import Dialog from "@material-ui/core/Dialog"
import DialogActions from "@material-ui/core/DialogActions"
import DialogContent from "@material-ui/core/DialogContent"
import DialogTitle from "@material-ui/core/DialogTitle"

import styled from "styled-components"

const StyledActivityButton = styled(Button)`
  color: #ffffff;
  border-radius: 2.0625rem;
  font-size: 0.875rem;
  text-transform: upper-case;
  letter-spacing: 0.03125rem;
  line-spacing: 1rem;
  padding: 1rem;
  flex-grow: 1;
`

export default class DetectionDialog extends Component {
  static propTypes = {
    isPlaying: bool.isRequired,
    feed: feedType.isRequired,
    timestamp: string.isRequired,
    getPlayerTime: func.isRequired
  }

  state = {
    open: false,
    submitted: false,
    description: ""
  }

  handleClickOpen = () => {
    gtag('event', "i_hear_something_interesting_button_pressed @" + this.props.feed.slug, {'event_category' : 'custom'})
    this.setState({ open: true })
  }

  handleChange = e => this.setState({ description: e.target.value })

  handleKeyDown = submitDetection => e => {
    if (e.which === 13) {
      this.onDetect(submitDetection)
    }
  }
  
  handleClose = () => {
    this.setState({
      open: false,
      submitted: false,
      description: ""
    })

      gtag('event', "detection_dialog_closed @" + this.props.feed.slug, {'event_category' : 'custom'})
  }

  onDetect = submitDetection => {
    const {
      feed: { id: feedId },
      timestamp: playlistTimestamp,
      isPlaying,
      getPlayerTime,
    } = this.props

    //const desc = "Testing"
    const type = "Orca"
  
    const playerOffset = getPlayerTime()
    const { description } = this.state
    if (feedId && playlistTimestamp && playerOffset && isPlaying) {
      submitDetection({
        variables: {
          feedId,
          playlistTimestamp,
          playerOffset,
          description,
          type
           // TODO FIX THIS FOR THE OTHER TYPES
        }
      })
    }
  }

  onSuccess = () => {
    this.setState({ submitted: true })
  }

  render() {
    return (
      <Mutation mutation={SUBMIT_DETECTION} onCompleted={this.onSuccess}>
        {(submitDetection, { data }) => (
          <>
            <StyledActivityButton
              color="secondary"
              onClick={this.handleClickOpen}
            >
              I hear something interesting
            </StyledActivityButton>
            <Dialog
              open={this.state.open}
              onClose={this.handleClose}
              aria-labelledby="form-dialog-title"
            >
              <DialogTitle id="form-dialog-title">
                {!this.state.submitted
                  ? "What do you think you heard?"
                  : "Thanks for submitting!"}
              </DialogTitle>
              {!this.state.submitted && (
                <DialogContent>
                  <TextField
                    autoFocus
                    margin="dense"
                    placeholder="Describe what you heard"
                    type="text"
                    fullWidth
                    onChange={this.handleChange}
                    onKeyDown={this.handleKeyDown(submitDetection)}
                  />
                </DialogContent>
              )}
              {!this.state.submitted ? (
                <DialogActions>
                  <Button onClick={this.handleClose} color="secondary">
                    CANCEL
                  </Button>
                  <Button
                    onClick={() => {
                      this.onDetect(submitDetection)
                      gtag('event', "detection_submitted @" + this.props.feed.slug, {'event_category' : 'custom'})

                    }}
                    color="secondary"
                  >
                    SUBMIT
                  </Button>
                </DialogActions>
              ) : (
                <DialogActions>
                  <Button onClick={this.handleShare} color="secondary" disabled>
                    SHARE
                  </Button>
                  <Button onClick={this.handleClose} color="secondary">
                    CLOSE
                  </Button>
                </DialogActions>
              )}
            </Dialog>
          </>
        )}
      </Mutation>
    )
  }
}
