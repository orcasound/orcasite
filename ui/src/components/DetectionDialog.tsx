import {
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Paper,
  TextField,
  ToggleButton,
  ToggleButtonGroup,
} from '@mui/material'
import Image from 'next/image'
import { useState } from 'react'

import vesselIconImage from '../../public/icons/vessel-purple.svg'
import wavesIconImage from '../../public/icons/water-waves-blue.svg'
import whaleFlukeIconImage from '../../public/icons/whale-fluke-gray.svg'

type DetectionCategory = 'orca' | 'vessel' | 'other'

export default function DetectionDialog(props: any) {
  const [open, setOpen] = useState(false)
  const [submitted, setSubmitted] = useState(false)
  const [category, setCategory] = useState<DetectionCategory>()
  const [description, setDescription] = useState('')

  const handleClickOpen = () => {
    setSubmitted(false)
    setDescription('')
    setCategory(undefined)
    setOpen(true)
  }

  const handleChange = (e: any) => setDescription(e.target.value)

  const handleCategoryChange = (e: any, newCategory: DetectionCategory) => {
    if (newCategory) {
      setCategory(newCategory)
    }
  }

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

  const categoryButtons = [
    { id: 'orca', iconImage: whaleFlukeIconImage },
    { id: 'vessel', iconImage: vesselIconImage },
    { id: 'other', iconImage: wavesIconImage },
  ]

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
            <ToggleButtonGroup
              value={category}
              exclusive
              onChange={handleCategoryChange}
              size="large"
              aria-label="Report sound"
              fullWidth
              sx={{
                marginY: 4,
              }}
            >
              {categoryButtons.map(({ id, iconImage }) => (
                <ToggleButton
                  value={id}
                  aria-label={id}
                  key={id}
                  component={Paper}
                  sx={{
                    '&&&': {
                      marginX: 5,
                      borderRadius: 1,
                      ':hover': {
                        borderColor: 'primary.main',
                      },
                    },
                    '&&.Mui-selected': {
                      border: 'solid 2px',
                      borderColor: 'primary.main',
                    },
                  }}
                >
                  <DetectionCategoryButton icon={iconImage} title={id} />
                </ToggleButton>
              ))}
            </ToggleButtonGroup>
            <TextField
              autoFocus
              margin="dense"
              placeholder="Describe what you heard (optional)"
              type="text"
              fullWidth
              onChange={handleChange}
              onKeyDown={handleKeyDown(submitDetection)}
              sx={{
                marginY: 2,
              }}
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
              disabled={!category}
            >
              SUBMIT
            </Button>
          </DialogActions>
        ) : (
          <DialogActions>
            <Button onClick={handleClose} color="primary">
              CLOSE
            </Button>
          </DialogActions>
        )}
      </Dialog>
    </>
  )
}

function DetectionCategoryButton({
  icon,
  title,
}: {
  icon: any
  title: string
}) {
  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
      }}
    >
      <Image src={icon.src} alt={`${title} icon`} width={100} height={100} />
      {title}
    </Box>
  )
}
