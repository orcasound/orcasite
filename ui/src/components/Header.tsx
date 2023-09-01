import { Close, Menu } from "@mui/icons-material";
import { AppBar, Box, IconButton, Toolbar, Typography } from "@mui/material";
import { useState } from "react";

import Link from "@/components/Link";
import { displayDesktopOnly, displayMobileOnly } from "@/styles/responsive";

export default function Header() {
  return (
    <AppBar
      position="static"
      sx={{
        // Keep header above the side drawer
        zIndex: (theme) => theme.zIndex.drawer + 1,
      }}
    >
      <Toolbar>
        <Mobile />
        <Desktop />
      </Toolbar>
    </AppBar>
  );
}

function Mobile() {
  const [menuIsOpen, setMenuOpen] = useState(false);

  const handleMenuToggle = () => {
    setMenuOpen(!menuIsOpen);
  };

  return (
    <Box sx={displayMobileOnly}>
      <Box
        sx={{
          flexGrow: 1,
          display: "grid",
          gridTemplateColumns: "repeat(3, 1fr)",
          alignItems: "center",
          textAlign: "center",
        }}
      >
        <IconButton
          sx={{ marginRight: "auto" }}
          color="inherit"
          onClick={handleMenuToggle}
        >
          {menuIsOpen ? <Close /> : <Menu />}
        </IconButton>
        <Brand />
      </Box>
    </Box>
  );
}

function Desktop() {
  return (
    <Box sx={displayDesktopOnly}>
      <Brand />
    </Box>
  );
}

function Brand() {
  return (
    <Typography variant="h6" noWrap>
      <Link href="/" color="inherit" underline="none">
        Orcasound
      </Link>
    </Typography>
  );
}
