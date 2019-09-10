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

class DetectionDialog extends Component {
  static propTypes = {
    isPlaying: bool.isRequired,
    feed: feedType.isRequired,
    timestamp: string.isRequired,
    getPlayerTime: func.isRequired
  }

  constructor(props) {
    super(props)

    this.state = {
      open: false,
      submitted: false,
      description: ""
    }
  }

  handleClickOpen = () => {
    this.setState({ open: true })
  }

  handleChange = e => {
    this.setState({ description: e.target.value })
  }

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
  }

  onDetect = submitDetection => {
    const {
      feed: { id: feedId },
      timestamp: playlistTimestamp,
      isPlaying,
      getPlayerTime,
      listenerCount
    } = this.props

    const playerOffset = getPlayerTime()
    const { description } = this.state
    if (feedId && playlistTimestamp && playerOffset && isPlaying) {
      submitDetection({
        variables: {
          feedId,
          playlistTimestamp,
          playerOffset,
          description,
          listenerCount
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
        {(submitDetection, { data }) => {
          return (
            <>
              <StyledActivityButton
                color="secondary"
                onClick={this.handleClickOpen}
              >
                I hear something interesting
              </StyledActivityButton>
              {!this.state.submitted ? (
                <Dialog
                  open={this.state.open}
                  onClose={this.handleClose}
                  aria-labelledby="form-dialog-title"
                >
                  <DialogTitle id="form-dialog-title">
                    What do you think you heard?
                  </DialogTitle>
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
                  <DialogActions>
                    <Button onClick={this.handleClose} color="secondary">
                      CANCEL
                    </Button>
                    <Button
                      onClick={() => {
                        this.onDetect(submitDetection)
                      }}
                      color="secondary"
                    >
                      SUBMIT
                    </Button>
                  </DialogActions>
                </Dialog>
              ) : (
                <Dialog
                  open={this.state.open}
                  aria-labelledby="form-dialog-title"
                >
                  <DialogTitle id="form-dialog-title">
                    Thanks for submitting!
                  </DialogTitle>
                  <DialogActions>
                    <Button
                      onClick={this.handleShare}
                      color="secondary"
                      disabled
                    >
                      SHARE
                    </Button>
                    <Button onClick={this.handleClose} color="secondary">
                      CLOSE
                    </Button>
                  </DialogActions>
                </Dialog>
              )}
            </>
          )
        }}
      </Mutation>
    )
  }
}

export default DetectionDialog
