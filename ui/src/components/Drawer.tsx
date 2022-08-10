import MenuIcon from '@mui/icons-material/Menu'
import {
  Drawer as SideDrawer,
  IconButton,
  SwipeableDrawer,
  Toolbar,
} from '@mui/material'
import { Box } from '@mui/system'
import { ReactNode, useState } from 'react'

import useIsMobile from '../hooks/useIsMobile'
import { BottomNavSpacer } from './BottomNav'

export default function Drawer({
  children,
  onOpen,
  onClose,
}: {
  children: ReactNode
  onOpen?: () => void
  onClose?: () => void
}) {
  const isMobile = useIsMobile()

  const [open, setOpen] = useState(true)

  const handleOpen = () => {
    setOpen(true)
    onOpen?.()
  }
  const handleClose = () => {
    setOpen(false)
    onClose?.()
  }

  return isMobile ? (
    <Mobile open={open} onOpen={handleOpen} onClose={handleClose}>
      {children}
    </Mobile>
  ) : (
    <Desktop open={open} onOpen={handleOpen} onClose={handleClose}>
      {children}
    </Desktop>
  )
}

type DrawerProps = {
  children: ReactNode
  open: boolean
  onOpen: () => void
  onClose: () => void
}

function Mobile({ children, open, onOpen, onClose }: DrawerProps) {
  return (
    <SwipeableDrawer
      anchor="bottom"
      open={open}
      onClose={onClose}
      onOpen={onOpen}
      swipeAreaWidth={100}
      disableSwipeToOpen={false}
      ModalProps={{
        keepMounted: true,
      }}
      sx={{
        '& > .MuiPaper-root': {
          height: 1,
          overflow: 'visible',
        },
      }}
    >
      <Box sx={{ overflow: 'auto' }}>
        <ToolbarSpacer />
        {children}
        <BottomNavSpacer />
      </Box>
    </SwipeableDrawer>
  )
}

function Desktop({ children, open, onOpen, onClose }: DrawerProps) {
  return (
    <SideDrawer
      variant="persistent"
      anchor="left"
      open={open}
      onClick={() => {
        if (!open) onOpen()
      }}
      sx={(theme) => ({
        width: theme.breakpoints.values.sm,
        maxWidth: 0.5,
        flexShrink: 0,
        transition: theme.transitions.create('width', {
          easing: theme.transitions.easing.easeOut,
          duration: theme.transitions.duration.enteringScreen,
        }),
        ...(!open && {
          width: 30,
          transition: theme.transitions.create('width', {
            easing: theme.transitions.easing.sharp,
            duration: theme.transitions.duration.leavingScreen,
          }),
        }),
        '& .MuiDrawer-paper': {
          width: theme.breakpoints.values.sm,
          maxWidth: 0.5,
          boxSizing: 'border-box',
        },
      })}
      ModalProps={{
        disablePortal: true,
        keepMounted: true,
      }}
    >
      <ToolbarSpacer />
      <Box sx={{ alignSelf: 'end' }}>
        {open ? (
          <IconButton onClick={onClose}>
            <MenuIcon />
          </IconButton>
        ) : (
          <IconButton onClick={onOpen}>
            <MenuIcon />
          </IconButton>
        )}
      </Box>
      {children}
    </SideDrawer>
  )
}

// Render a second toolbar to deal with spacing on fixed AppBar
// https://mui.com/components/app-bar/#fixed-placement
function ToolbarSpacer() {
  return <Toolbar />
}
