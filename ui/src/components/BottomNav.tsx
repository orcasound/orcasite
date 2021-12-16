import GraphicEqIcon from '@mui/icons-material/GraphicEq'
import InfoIcon from '@mui/icons-material/Info'
import MenuIcon from '@mui/icons-material/Menu'
import NotificationsIcon from '@mui/icons-material/Notifications'
import { BottomNavigation, BottomNavigationAction, Paper } from '@mui/material'

import useIsMobile from '../hooks/useIsMobile'

export default function BottomNav() {
  const isMobile = useIsMobile()
  return isMobile ? (
    <Paper
      sx={{
        position: 'fixed',
        bottom: 0,
        left: 0,
        right: 0,
      }}
    >
      <BottomNavigation showLabels sx={{ height: (theme) => theme.spacing(8) }}>
        <BottomNavigationAction label="Info" icon={<InfoIcon />} />
        <BottomNavigationAction label="Listen" icon={<GraphicEqIcon />} />
        <BottomNavigationAction
          label="Subscribe"
          icon={<NotificationsIcon />}
        />
        <BottomNavigationAction label="More" icon={<MenuIcon />} />
      </BottomNavigation>
    </Paper>
  ) : null
}
