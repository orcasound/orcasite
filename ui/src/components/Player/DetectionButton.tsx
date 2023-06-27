import { GraphicEq } from '@mui/icons-material'
import { Box, Fab } from '@mui/material'

export default function DetectionButton() {
  return (
    <Fab
      variant="extended"
      size="large"
      color="secondary"
      sx={{
        position: 'absolute',
        bottom: 100,
        left: 0,
        right: 0,
        margin: 'auto',
        maxWidth: 'max-content',

        // style to look like outlined button
        backgroundColor: 'white',
        color: 'primary.main',
        borderColor: 'primary.main',
        borderStyle: 'solid',
        borderWidth: '2px',
        '&:hover': {
          backgroundColor: 'primary.main',
          color: 'white',
        },
      }}
    >
      <Box
        sx={{
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
        }}
      >
        <GraphicEq sx={{ mr: 1 }} />
        Report sound
      </Box>
    </Fab>
  )
}
