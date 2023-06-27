import { Pause, PlayArrow } from '@mui/icons-material'
import { Fab } from '@mui/material'

export default function PlayPauseButton({
  isPlaying,
  onClick,
}: {
  isPlaying: boolean
  onClick: () => void
}) {
  return (
    <Fab
      color="base"
      sx={{
        // set hover color manually because custom colors are broken for Fab
        // see https://github.com/mui/material-ui/issues/31063
        '&:hover': {
          backgroundColor: 'base.light',
        },
      }}
      onClick={onClick}
    >
      {!isPlaying && (
        <PlayArrow
          className="icon"
          fontSize="large"
          // onClick={() =>
          //   analyticsEvents.stream.started(currentFeed.slug)
          // }
        />
      )}
      {isPlaying && (
        <Pause
          className="icon"
          fontSize="large"
          // onClick={() =>
          //   analyticsEvents.stream.paused(currentFeed.slug)
          // }
        />
      )}
    </Fab>
  )
}
