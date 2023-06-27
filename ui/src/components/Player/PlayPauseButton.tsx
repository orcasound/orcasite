import { ErrorOutline, Pause, PlayArrow } from '@mui/icons-material'
import { CircularProgress, Fab, Tooltip } from '@mui/material'

export default function PlayPauseButton({
  isPlaying,
  isLoading,
  isError,
  disabled,
  onClick,
}: {
  isPlaying: boolean
  isLoading: boolean
  isError: boolean
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
      disabled={isLoading && !isError}
    >
      {isError ? (
        <Tooltip title="Failed to load" placement="right">
          <ErrorOutline className="icon" fontSize="large" />
        </Tooltip>
      ) : isLoading ? (
        <CircularProgress sx={{ color: 'base.contrastText' }} />
      ) : isPlaying ? (
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
