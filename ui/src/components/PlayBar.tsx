import {
  Close,
  Feedback,
  Home,
  Menu,
  Notifications,
} from "@mui/icons-material";
import {
  AppBar,
  Box,
  Button,
  Divider,
  Drawer,
  IconButton,
  List,
  ListItem,
  ListItemButton,
  ListItemIcon,
  ListItemText,
  Toolbar,
  Typography,
} from "@mui/material";
import Image from "next/image";
import { useEffect, useState } from "react";

import Link from "@/components/Link";
import { useFeedsQuery } from "@/graphql/generated";
import { Candidate } from "@/pages/moderator/candidates";
import wordmark from "@/public/wordmark/wordmark-white.svg";
import { displayDesktopOnly, displayMobileOnly } from "@/styles/responsive";
import { analytics } from "@/utils/analytics";

import { CandidateCardPlayer } from "./Player/CandidateCardPlayer";

export default function PlayBar({ candidate }: { candidate: Candidate }) {
  // get hydrophone feed list
  const feedsQueryResult = useFeedsQuery();
  const feedsData = feedsQueryResult.data?.feeds ?? [];

  const [playerProps, setPlayerProps] = useState({
    feed: feedsData[0],
    timestamp: 0,
    startOffset: 0,
    endOffset: 0,
  });

  useEffect(() => {
    const candidateArray = candidate.array;
    if (candidateArray) {
      const firstCandidate = candidateArray[candidateArray.length - 1];
      const lastCandidate = candidateArray[0];
      const feed = feedsData.find((feed) => feed.id === firstCandidate.feedId);

      const startTimestamp = Math.min(
        ...candidateArray.map((d) => +d.playlistTimestamp),
      );

      const offsetPadding = 15;
      const minOffset = Math.min(...candidateArray.map((d) => +d.playerOffset));

      // const maxOffset = Math.max(...candidateArray.map((d) => +d.playerOffset));
      // instead, ensure that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
      const firstPlaylist = candidateArray.filter(
        (d) => +d.playlistTimestamp === startTimestamp,
      );

      const maxOffset = Math.max(...firstPlaylist.map((d) => +d.playerOffset));
      const startOffset = Math.max(0, minOffset - offsetPadding);
      const endOffset = maxOffset + offsetPadding;

      feed &&
        setPlayerProps({
          feed: feed ? feed : feedsData[0],
          timestamp: startTimestamp,
          startOffset: startOffset,
          endOffset: endOffset,
        });
    }
  }, [candidate]);

  return (
    <AppBar
      // position="static"
      position="fixed"
      sx={{
        // Keep header above the side drawer
        zIndex: (theme) => theme.zIndex.drawer + 1,
        bottom: 0,
        top: "auto",
        height: "100px",
      }}
    >
      <Toolbar>
        {/* <pre style={{color: "white"}}>{JSON.stringify(candidate)}</pre> */}
        {candidate.array && playerProps.feed ? (
          <CandidateCardPlayer
            feed={playerProps.feed}
            timestamp={playerProps.timestamp}
            startOffset={playerProps.startOffset}
            endOffset={playerProps.endOffset}
          />
        ) : (
          "No recordings loaded"
        )}
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
          {menuIsOpen ? <Close /> : <Menu />}
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
          }}
          sx={{
            display: { xs: "block", sm: "none" },
            "& .MuiDrawer-paper": {
              boxSizing: "border-box",
              width: drawerWidth,
              backgroundColor: "base.main",
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
            <Box sx={{ my: 2 }}>
              <Image
                src={wordmark.src}
                alt="Orcasound"
                width={140}
                height={60}
                priority={true}
              />
            </Box>
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
                        displa: "flex",
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

function Desktop() {
  const pages = [
    {
      label: "About us",
      url: "https://www.orcasound.net/",
      onClick: () => analytics.nav.aboutTabClicked(),
    },
    {
      label: "Send feedback",
      url: "https://forms.gle/wKpAnxzUh9a5LMfd7",
      onClick: () => analytics.nav.feedbackTabClicked(),
    },
  ];
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
        <Box sx={{ display: "flex" }}>
          {pages.map((page) => (
            <Button
              onClick={() => page.onClick && page.onClick()}
              href={page.url}
              target="_blank"
              key={page.label}
              sx={{
                my: 2,
                mx: 1,
                color: "base.contrastText",
                display: "block",
              }}
            >
              {page.label}
            </Button>
          ))}
          <Link
            href="https://docs.google.com/forms/d/1oYSTa3QeAAG-G_eTxjabrXd264zVARId9tp2iBRWpFs/edit"
            title="Get notified when there's whale activity."
            target="_blank"
            sx={{
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
              textDecoration: "none",
              textTransform: "uppercase",
              "&:hover": { color: "#ccc" },
              mx: 1,
            }}
            onClick={() => analytics.nav.notificationsClicked()}
          >
            <Button
              variant="outlined"
              // color="info"
              startIcon={<Notifications />}
              sx={{
                borderRadius: 8,
                backgroundColor: "white",
                "&:hover": {
                  borderColor: "#a2d1cf",
                  backgroundColor: "#efefef",
                },
                "&:focus": {
                  borderColor: "#a2d1cf",
                  backgroundColor: "#efefef",
                },
              }}
            >
              Get notified
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
