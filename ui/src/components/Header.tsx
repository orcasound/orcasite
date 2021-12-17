import CloseIcon from '@mui/icons-material/Close'
import MenuIcon from '@mui/icons-material/Menu'
import { AppBar, Box, IconButton, Toolbar, Typography } from '@mui/material'
import { useState } from 'react'

import useIsMobile from '../hooks/useIsMobile'
import Link from './Link'

export default function Header() {
  const isMobile = useIsMobile()
  return (
    <Box>
      <AppBar position="fixed">
        <Toolbar>{isMobile ? <Mobile /> : <Desktop />}</Toolbar>
      </AppBar>
      {/* Render a second toolbar to deal with spacing on fixed AppBar
          https://mui.com/components/app-bar/#fixed-placement */}
      <Toolbar />
    </Box>
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
