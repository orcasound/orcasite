import CloseIcon from '@mui/icons-material/Close'
import MenuIcon from '@mui/icons-material/Menu'
import { AppBar, Box, IconButton, Toolbar, Typography } from '@mui/material'
import { useState } from 'react'

import useIsMobile from '../hooks/useIsMobile'
import Link from './Link'

export default function Header() {
  const isMobile = useIsMobile()
  return (
    <AppBar
      position="static"
      sx={{
        // Keep header above the side drawer
        zIndex: (theme) => theme.zIndex.drawer + 1,
      }}
    >
      <Toolbar>{isMobile ? <Mobile /> : <Desktop />}</Toolbar>
    </AppBar>
  )
}

function Mobile() {
  const [menuIsOpen, setMenuOpen] = useState(false)

  const handleMenuToggle = () => {
    setMenuOpen(!menuIsOpen)
  }

  return (
    <Box
      sx={{
        flexGrow: 1,
        display: 'grid',
        gridTemplateColumns: 'repeat(3, 1fr)',
        alignItems: 'center',
        textAlign: 'center',
      }}
    >
      <IconButton
        sx={{ marginRight: 'auto' }}
        color="inherit"
        onClick={handleMenuToggle}
      >
        {menuIsOpen ? <CloseIcon /> : <MenuIcon />}
      </IconButton>
      <Brand />
    </Box>
  )
}

function Desktop() {
  return <Brand />
}

function Brand() {
  return (
    <Typography variant="h6" noWrap>
      <Link href="/" color="inherit" underline="none">
        Orcasound
      </Link>
    </Typography>
  )
}
