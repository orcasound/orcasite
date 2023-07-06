import { ErrorOutline, Pause, PlayArrow } from '@mui/icons-material'
import { CircularProgress, Fab, Tooltip } from '@mui/material'

import { type PlayerStatus } from './Player'

export default function PlayPauseButton({
  playerStatus,
  disabled,
  onClick,
}: {
  playerStatus: PlayerStatus
  disabled: boolean
  onClick: () => void
}) {
  return (
    <Fab
      color="base"
      sx={
        disabled
          ? { color: 'base.light' }
          : {
              // set hover color manually because custom colors are broken for Fab
              // see https://github.com/mui/material-ui/issues/31063
              '&:hover': {
                backgroundColor: 'base.light',
              },
            }
      }
      disableRipple={disabled}
      onClick={() => !disabled && onClick()}
    >
      {playerStatus === 'error' ? (
        <Tooltip title="Failed to load" placement="right">
          <ErrorOutline className="icon" fontSize="large" />
        </Tooltip>
      ) : playerStatus === 'loading' ? (
        <CircularProgress sx={{ color: 'base.contrastText' }} />
      ) : playerStatus === 'playing' ? (
        <Pause
          className="icon"
          fontSize="large"
          // onClick={() =>
          //   analyticsEvents.stream.paused(currentFeed.slug)
          // }
        />
      ) : (
        <PlayArrow
          className="icon"
          fontSize="large"
          // onClick={() =>
          //   analyticsEvents.stream.started(currentFeed.slug)
          // }
        />
      )}
    </Fab>
  )
}
