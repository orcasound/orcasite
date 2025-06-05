import { Close, Tune } from "@mui/icons-material";
import {
  Alert,
  AppBar,
  Box,
  Button,
  Collapse,
  Drawer,
  IconButton,
  List,
  Toolbar,
  Typography,
} from "@mui/material";
import Image from "next/image";
import { ReactNode, useState } from "react";

import Link from "@/components/Link";
import wordmark from "@/public/wordmark/wordmark-white.svg";
import { displayDesktopOnly, displayMobileOnly } from "@/styles/responsive";
import { analytics } from "@/utils/analytics";

import CandidateListFilters from "./CandidateList/CandidateListFilters";

export default function HeaderNew({
  onBrandClick,
  tabs,
}: {
  onBrandClick?: () => void;
  tabs?: ReactNode;
}) {
  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const [alertOpen, setAlertOpen] = useState(true);

  return (
    <AppBar
      // position="sticky"
      // position={mdDown ? "fixed" : "static"} // needed this to be fixed to avoid issue with 100vh preventing scroll on mobile
      position={"static"}
      sx={{
        // Keep header above the side drawer
        zIndex: (theme) => theme.zIndex.drawer + 1,
      }}
    >
      <Collapse in={alertOpen}>
        <Alert
          severity="info"
          variant="filled"
          action={
            <IconButton
              aria-label="close"
              color="inherit"
              size="small"
              onClick={() => {
                setAlertOpen(false);
              }}
            >
              <Close fontSize="inherit" />
            </IconButton>
          }
          sx={{
            borderRadius: 0,
            justifyContent: "center",
            paddingY: 0,
            "& .MuiAlert-action": {
              marginLeft: 0,
            },
          }}
        >
          DO THIS THING
        </Alert>
      </Collapse>{" "}
      <Toolbar>
        <Mobile onBrandClick={onBrandClick} />
        <Desktop tabs={tabs} />
      </Toolbar>
    </AppBar>
  );
}

function Mobile({
  window,
  onBrandClick,
}: {
  window?: () => Window;
  onBrandClick?: () => void;
}) {
  const drawerWidth = "100%";
  const [menuIsOpen, setMenuOpen] = useState(false);

  const handleMenuToggle = () => {
    setMenuOpen(!menuIsOpen);
  };

  const container =
    window !== undefined ? () => window().document.body : undefined;

  return (
    <Box sx={displayMobileOnly} width={1}>
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
          sx={{
            marginRight: "auto",
            backgroundColor: "rgba(255,255,255,.15)",
            padding: "6px !important",
            borderRadius: "4px",
          }}
          color="inherit"
          onClick={handleMenuToggle}
        >
          {menuIsOpen ? <Close /> : <Tune />}
        </IconButton>
        <Brand onClick={onBrandClick} />
      </Box>
      <nav>
        <Drawer
          container={container}
          variant="temporary"
          open={menuIsOpen}
          onClose={handleMenuToggle}
          ModalProps={{
            keepMounted: true, // Better open performance on mobile.
            BackdropProps: {
              style: { backgroundColor: "transparent" },
            },
          }}
          sx={{
            display: { xs: "block", sm: "none" },
            "& .MuiDrawer-paper": {
              boxSizing: "border-box",
              width: drawerWidth,
              backgroundColor: "base.main",
              marginTop: (theme) => `${theme.mixins.toolbar.minHeight}px`,
            },
            zIndex: (theme) => theme.zIndex.drawer + 1,
          }}
        >
          <Box
            onClick={handleMenuToggle}
            sx={{ textAlign: "center", height: "100%" }}
            display="flex"
            flexDirection="column"
            alignItems="center"
          >
            <List sx={{ maxWidth: (theme) => theme.breakpoints.values.sm }}>
              <CandidateListFilters />
            </List>
          </Box>
        </Drawer>
      </nav>
    </Box>
  );
}

function Desktop({ tabs }: { tabs?: ReactNode }) {
  return (
    <Box sx={{ ...displayDesktopOnly, width: "100%" }}>
      <Box
        sx={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          width: 1,
        }}
      >
        <Brand />

        <Box sx={{ flex: 1, marginLeft: "3rem" }}>{tabs}</Box>

        <Box className="header-buttons" sx={{ display: "flex", gap: "16px" }}>
          <Box sx={{ width: "260px" }}>
            <CandidateListFilters />
          </Box>
          <Link href="#" onClick={() => analytics.nav.notificationsClicked()}>
            <Button
              size="small"
              variant="outlined"
              sx={{ whiteSpace: "nowrap" }}
            >
              Sign up
            </Button>
          </Link>
          <Link href="#">
            <Button size="small" variant="contained">
              Log in
            </Button>
          </Link>
        </Box>
      </Box>
    </Box>
  );
}

function Brand({ onClick }: { onClick?: () => void }) {
  return (
    <Typography variant="h6" noWrap overflow="visible">
      <Link
        href="/"
        color="inherit"
        underline="none"
        sx={{
          height: "100%",
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
        }}
        onClick={() => {
          if (onClick) onClick();
          analytics.nav.logoClicked();
        }}
      >
        <Image
          src={wordmark.src}
          alt="Orcasound"
          width={140}
          height={60}
          priority={true}
        />
      </Link>
    </Typography>
  );
}
