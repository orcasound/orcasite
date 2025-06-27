import {
  Close,
  Feedback,
  Home,
  Menu as MenuIcon,
  Notifications,
} from "@mui/icons-material";
import {
  Box,
  Divider,
  Drawer,
  IconButton,
  List,
  ListItem,
  ListItemButton,
  ListItemIcon,
  ListItemText,
} from "@mui/material";
import Image from "next/image";
import { useState } from "react";

import wordmark from "@/public/wordmark/wordmark-white.svg";
import { displayMobileOnly } from "@/styles/responsive";
import { analytics } from "@/utils/analytics";

import UserMenu from "./AccountMenu";
import Brand from "./Brand";

export default function Mobile({
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

  const navItems = [
    {
      label: "About us",
      url: "https://www.orcasound.net/",
      ItemIcon: Home,
      onClick: () => analytics.nav.aboutTabClicked(),
    },
    {
      label: "Get notified",
      url: "https://docs.google.com/forms/d/1oYSTa3QeAAG-G_eTxjabrXd264zVARId9tp2iBRWpFs/edit",
      ItemIcon: Notifications,
      onClick: () => analytics.nav.notificationsClicked(),
    },
    {
      label: "Send feedback",
      url: "https://forms.gle/wKpAnxzUh9a5LMfd7",
      ItemIcon: Feedback,
      onClick: () => analytics.nav.feedbackTabClicked(),
    },
  ];

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
          sx={{ marginRight: "auto" }}
          color="inherit"
          onClick={handleMenuToggle}
        >
          {menuIsOpen ? <Close /> : <MenuIcon />}
        </IconButton>
        <Brand onClick={onBrandClick} />
        <UserMenu sx={{ marginLeft: "auto", paddingLeft: 1 }} />
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
            <Divider color="base.contrastText" />
            <List sx={{ maxWidth: (theme) => theme.breakpoints.values.sm }}>
              {navItems.map((item) => (
                <ListItem key={item.label} disablePadding>
                  <ListItemButton
                    href={item.url}
                    target="_blank"
                    onClick={() => item.onClick && item.onClick()}
                  >
                    <ListItemIcon
                      sx={{
                        color: "base.contrastText",
                        display: "flex",
                        justifyContent: "center",
                        opacity: 0.9,
                      }}
                    >
                      <item.ItemIcon />
                    </ListItemIcon>
                    <ListItemText
                      primary={item.label}
                      sx={{
                        color: "base.contrastText",
                        textTransform: "uppercase",
                      }}
                    />
                  </ListItemButton>
                </ListItem>
              ))}
            </List>
          </Box>
        </Drawer>
      </nav>
    </Box>
  );
}
