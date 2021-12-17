import { Drawer as SideDrawer, SwipeableDrawer } from '@mui/material'
import { Box } from '@mui/system'
import { ReactNode, useState } from 'react'

import useIsMobile from '../hooks/useIsMobile'
import { BottomNavSpacer } from './BottomNav'
import { ToolbarSpacer } from './Header'

export default function Drawer({ children }: { children: ReactNode }) {
  const isMobile = useIsMobile()

  const [open, setOpen] = useState(true)

  return isMobile ? (
    <Mobile open={open} setOpen={setOpen}>
      {children}
    </Mobile>
  ) : (
    <Desktop open={open} setOpen={setOpen}>
      {children}
    </Desktop>
  )
}

type DrawerProps = {
  children: ReactNode
  open: boolean
  setOpen: (open: boolean) => void
}

function Mobile({ children, open, setOpen }: DrawerProps) {
  return (
    <SwipeableDrawer
      anchor="bottom"
      open={open}
      onClose={() => setOpen(false)}
      onOpen={() => setOpen(true)}
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

function Desktop({ children, open, setOpen }: DrawerProps) {
  return (
    <SideDrawer
      variant="persistent"
      anchor="left"
      open={open}
      sx={{
        '& .MuiDrawer-paper': {
          width: (theme) => theme.breakpoints.values.sm,
          maxWidth: 0.5,
        },
      }}
    >
      <ToolbarSpacer />
      {children}
    </SideDrawer>
  )
}
