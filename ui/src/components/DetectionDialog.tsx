import {
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  TextField,
} from '@mui/material'
import { useState } from 'react'

export default function DetectionDialog(props: any) {
  const [open, setOpen] = useState(false)
  const [submitted, setSubmitted] = useState(false)
  const [description, setDescription] = useState('')

  const handleClickOpen = () => {
    setSubmitted(false)
    setDescription('')
    setOpen(true)
  }

  const handleChange = (e: any) => setDescription(e.target.value)

  const handleKeyDown = (submitDetection: any) => (e: any) => {
    if (e.which === 13) {
      onDetect(submitDetection)
    }
  }

  const handleClose = () => {
    setOpen(false)
  }

  const onDetect = (submitDetection: any) => {
    const {
      feed: { id: feedId },
      timestamp: playlistTimestamp,
      isPlaying,
      getPlayerTime,
      listenerCount,
    } = props

    const playerOffset = getPlayerTime()
    if (feedId && playlistTimestamp && playerOffset && isPlaying) {
      submitDetection({
        variables: {
          feedId,
          playlistTimestamp,
          playerOffset,
          description,
          listenerCount,
        },
      })
    }
  }

  const onSuccess = () => {
    setSubmitted(true)
  }

  function submitDetection(data: any) {
    console.log('submitDetection')
    console.log(data)
    onSuccess()
  }

  return (
    <>
      <Box onClick={handleClickOpen}>{props.children}</Box>
      <Dialog
        open={open}
        onClose={handleClose}
        aria-labelledby="form-dialog-title"
      >
        <DialogTitle id="form-dialog-title">
          {!submitted ? 'Report what you heard' : 'Thanks for submitting!'}
        </DialogTitle>
        {!submitted && (
          <DialogContent>
            <TextField
              autoFocus
              margin="dense"
              placeholder="Describe what you heard"
              type="text"
              fullWidth
              onChange={handleChange}
              onKeyDown={handleKeyDown(submitDetection)}
            />
          </DialogContent>
        )}
        {!submitted ? (
          <DialogActions>
            <Button onClick={handleClose} color="primary">
              CANCEL
            </Button>
            <Button
              onClick={() => {
                onDetect(submitDetection)
              }}
              color="primary"
              variant="outlined"
            >
              SUBMIT
            </Button>
          </DialogActions>
        ) : (
          <DialogActions>
            <Button color="primary" disabled>
              SHARE
            </Button>
            <Button onClick={handleClose} color="primary">
              CLOSE
            </Button>
          </DialogActions>
        )}
      </Dialog>
    </>
  )
}
