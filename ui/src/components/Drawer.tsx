import { KeyboardArrowDown, Menu } from "@mui/icons-material";
import {
  Box,
  Drawer as SideDrawer,
  IconButton,
  SwipeableDrawer,
  Toolbar,
  Typography,
} from "@mui/material";
import { ReactNode } from "react";

import useIsMobile from "@/hooks/useIsMobile";
import { displayDesktopOnly, displayMobileOnly } from "@/styles/responsive";

import { PlayerSpacer } from "./Player";

export default function Drawer({
  children,
  open,
  setOpen,
  onOpen,
  onClose,
}: {
  children: ReactNode;
  open: boolean;
  setOpen: (open: boolean) => void;
  onOpen?: () => void;
  onClose?: () => void;
}) {
  const isMobile = useIsMobile();

  const handleOpen = () => {
    setOpen(true);
    onOpen?.();
  };
  const handleClose = () => {
    setOpen(false);
    onClose?.();
  };
  return (
    <>
      <Mobile open={open && isMobile} onOpen={handleOpen} onClose={handleClose}>
        {children}
      </Mobile>

      <Desktop open={open} onOpen={handleOpen} onClose={handleClose}>
        {children}
      </Desktop>
    </>
  );
}

type DrawerProps = {
  children: ReactNode;
  open: boolean;
  onOpen: () => void;
  onClose: () => void;
};

function Mobile({ children, open, onOpen, onClose }: DrawerProps) {
  return (
    <SwipeableDrawer
      anchor="bottom"
      open={open}
      onClose={onClose}
      onOpen={onOpen}
      swipeAreaWidth={80}
      disableSwipeToOpen={false}
      SwipeAreaProps={{
        sx: displayMobileOnly,
      }}
      ModalProps={{
        keepMounted: true,
      }}
      sx={{
        ...displayMobileOnly,
        "& > .MuiPaper-root": {
          height: 1,
          minHeight: "150px",
        },
      }}
    >
      <Box sx={{ overflow: "auto" }}>
        <ToolbarSpacer />
        <Box display="flex" justifyContent="space-between" alignItems="center">
          <Typography variant="h4" mt={4} ml={2}>
            Listen live
          </Typography>
          {open ? (
            <IconButton onClick={onClose}>
              <KeyboardArrowDown fontSize="large" />
            </IconButton>
          ) : (
            <IconButton onClick={onOpen}>
              <Menu />
            </IconButton>
          )}
        </Box>
        {children}
        <PlayerSpacer />
      </Box>
    </SwipeableDrawer>
  );
}

function Desktop({ children, open }: DrawerProps) {
  return (
    <SideDrawer
      variant="persistent"
      anchor="left"
      open={open}
      sx={(theme) => ({
        ...displayDesktopOnly,
        width: theme.breakpoints.values.sm,
        maxWidth: 0.5,
        flexShrink: 0,
        transition: theme.transitions.create("width", {
          easing: theme.transitions.easing.easeOut,
          duration: theme.transitions.duration.enteringScreen,
        }),
        ...(!open && {
          width: 0,
          transition: theme.transitions.create("width", {
            easing: theme.transitions.easing.sharp,
            duration: theme.transitions.duration.leavingScreen,
          }),
        }),
        "& .MuiDrawer-paper": {
          width: theme.breakpoints.values.sm,
          maxWidth: 0.5,
          boxSizing: "border-box",
        },
      })}
      ModalProps={{
        disablePortal: true,
        keepMounted: true,
      }}
    >
      <ToolbarSpacer />
      <Typography variant="h4" mt={4} ml={3}>
        Listen live
      </Typography>
      {children}
    </SideDrawer>
  );
}

// Render a second toolbar to deal with spacing on fixed AppBar
// https://mui.com/components/app-bar/#fixed-placement
function ToolbarSpacer() {
  return <Toolbar />;
}
