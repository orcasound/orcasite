import { AppBar, Box, Toolbar, Typography } from '@mui/material'

export default function Header() {
  return (
    <Box>
      <AppBar position="sticky">
        <Toolbar>
          <Typography variant="h6" sx={{ textTransform: 'uppercase' }}>
            Orcasound
          </Typography>
        </Toolbar>
      </AppBar>
    </Box>
  )
}
