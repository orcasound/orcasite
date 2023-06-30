import { GraphicEq, Info, Menu, Notifications } from '@mui/icons-material'
import {
  BottomNavigation,
  BottomNavigationAction,
  Box,
  Paper,
} from '@mui/material'
import { styled } from '@mui/material/styles'

import useIsMobile from '@/hooks/useIsMobile'

export default function BottomNav() {
  const isMobile = useIsMobile()
  return isMobile ? (
    <Paper
      sx={{
        position: 'fixed',
        bottom: 0,
        left: 0,
        right: 0,
        // Keep nav above the sliding drawer
        zIndex: (theme) => theme.zIndex.drawer + 1,
      }}
    >
      <BottomNavigation showLabels sx={{ height: (theme) => theme.spacing(8) }}>
        <BottomNavigationAction label="Info" icon={<Info />} />
        <BottomNavigationAction label="Listen" icon={<GraphicEq />} />
        <BottomNavigationAction label="Subscribe" icon={<Notifications />} />
        <BottomNavigationAction label="More" icon={<Menu />} />
      </BottomNavigation>
    </Paper>
  ) : null
}

// Utility component to help with spacing
// Just a box that's the same height as bottom nav
export const BottomNavSpacer = styled(Box)(({ theme }) => ({
  height: theme.spacing(8),
}))
