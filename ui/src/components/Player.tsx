import { Box } from '@mui/material'

import { Feed } from '../generated/types'

export default function Player({ currentFeed }: { currentFeed?: Feed }) {
  return (
    <Box sx={{ minHeight: 80, backgroundColor: 'gray' }}>
      {currentFeed?.nodeName ?? 'Player: no feed selected'}
    </Box>
  )
}
