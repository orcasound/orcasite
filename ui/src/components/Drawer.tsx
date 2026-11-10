import {
  Box,
  Drawer as SideDrawer,
  SwipeableDrawer,
  Toolbar,
} from "@mui/material";
import { ReactNode } from "react";

import useIsMobile from "@/hooks/useIsMobile";
import { displayDesktopOnly, displayMobileOnly } from "@/styles/responsive";

import { PlayerSpacer } from "./Player";

/** Responsive drawer with an optional mobile footer outside the scrolling content. */
export default function Drawer({
  children,
  open,
  setOpen,
  onOpen,
  onClose,
  mobileFooter,
}: {
  children: ReactNode;
  open: boolean;
  setOpen: (open: boolean) => void;
  onOpen?: () => void;
  onClose?: () => void;
  mobileFooter?: ReactNode;
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
      <Mobile
        open={open && isMobile}
        onOpen={handleOpen}
        onClose={handleClose}
        footer={mobileFooter}
      >
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

/** Keeps the footer and player space visible while the drawer content scrolls. */
function Mobile({
  children,
  open,
  onOpen,
  onClose,
  footer,
}: DrawerProps & { footer?: ReactNode }) {
  return (
    <SwipeableDrawer
      anchor="bottom"
      open={open}
      onClose={onClose}
      onOpen={onOpen}
      swipeAreaWidth={80}
      disableSwipeToOpen={false}
      slotProps={{
        swipeArea: { sx: displayMobileOnly },
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
      <Box sx={{ overflow: "auto", flexGrow: 1, minHeight: 0 }}>
        <ToolbarSpacer />
        {children}
      </Box>
      {footer}
      <PlayerSpacer sx={{ flexShrink: 0 }} />
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
      {children}
    </SideDrawer>
  );
}

// Render a second toolbar to deal with spacing on fixed AppBar
// https://mui.com/components/app-bar/#fixed-placement
function ToolbarSpacer() {
  return <Toolbar />;
}
