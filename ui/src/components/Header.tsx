import { Close, Menu, Notifications } from "@mui/icons-material";
import { AppBar, Box, IconButton, Toolbar, Typography } from "@mui/material";
import Image from "next/image";
import { useState } from "react";

import Link from "@/components/Link";
import wordmark from "@/public/wordmark/wordmark-white.svg";
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
    <Box sx={{ ...displayDesktopOnly, width: "100%" }}>
      <Box
        sx={{
          display: "flex",
          justifyContent: "flex-start",
          alignItems: "center",
        }}
      >
        <Brand />
        <Box sx={{ marginLeft: "auto" }}>
          <Link
            href="https://docs.google.com/forms/d/1oYSTa3QeAAG-G_eTxjabrXd264zVARId9tp2iBRWpFs/edit"
            title="Get notified when there's whale activity."
            sx={{
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
              color: "white",
              textDecoration: "none",
              textTransform: "uppercase",
              "&:hover": { color: "#ccc" },
            }}
          >
            <IconButton color="inherit">
              <Notifications />
            </IconButton>
          </Link>
        </Box>
      </Box>
    </Box>
  );
}

function Brand() {
  return (
    <Typography variant="h6" noWrap>
      <Link
        href="/"
        color="inherit"
        underline="none"
        sx={{ height: "100%", display: "flex", alignItems: "center" }}
      >
        <Image src={wordmark.src} alt="Orcasound" width={140} height={60} />
      </Link>
    </Typography>
  );
}
