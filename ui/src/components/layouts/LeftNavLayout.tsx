import type { Theme } from "@mui/material";
import { Box, Stack, Typography } from "@mui/material";
import { useMediaQuery } from "@mui/material";
import Divider from "@mui/material/Divider";
import Drawer from "@mui/material/Drawer";
import List from "@mui/material/List";
import ListItem from "@mui/material/ListItem";
import ListItemButton from "@mui/material/ListItemButton";
import ListItemIcon from "@mui/material/ListItemIcon";
import ListSubheader from "@mui/material/ListSubheader";
import Image from "next/image";
import * as React from "react";
import { ReactElement } from "react";

import Link from "@/components/Link";
import { LayoutContext } from "@/context/LayoutContext";
import wordmark from "@/public/wordmark/wordmark-white.svg";
import { analytics } from "@/utils/analytics";

import PlayBar from "../PlayBar";
import { TopNav } from "./Devias-dashboard/vertical-layout/top-nav";
import { MasterDataLayout } from "./MasterDataLayout";
import navigationList from "./navigationList";

// const drawerWidth = 240;
const drawerWidth = "280px";

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

function LeftNavLayout({ children }: { children: React.ReactNode }) {
  //// MOBILE
  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  //// COMPONENTS

  const listItem = (
    title: string,
    path: string,
    icon: ReactElement,
    key: string,
  ) => (
    <Link
      key={key}
      href={path}
      underline="none"
      width={0.9}
      sx={{
        color: "inherit",
        opacity: ".75",
        ["&.active"]: {
          background: "rgba(255,255,255,.15)",
          opacity: "1",
          // background: "#258DAD",
        },
      }}
    >
      <ListItem
        disablePadding
        sx={{
          background: "inherit",
          borderRadius: "8px",
        }}
      >
        <ListItemButton style={{ padding: "14px 20px" }}>
          <ListItemIcon
            style={{
              color: "inherit",
              minWidth: "48px",
              fontSize: "27px",
              opacity: ".75",
            }}
          >
            {icon}
          </ListItemIcon>
          <Typography
            sx={{
              margin: 0,
              fontWeight: "inherit",
              fontFamily: "inherit",
              fontSize: "inherit",
            }}
          >
            {title}
          </Typography>
        </ListItemButton>
      </ListItem>
    </Link>
  );

  const subheader = (content: string) => (
    <ListSubheader
      component="div"
      id={content.replace(" ", "-").toLowerCase()}
      sx={{
        background: "transparent",
        color: "inherit",
        textTransform: "uppercase",
        fontWeight: "bold",
        opacity: ".75",
        lineHeight: 2.5,
        marginTop: "1.2rem",
        fontSize: "14px",
      }}
    >
      {content}
    </ListSubheader>
  );

  interface NavDiv {
    title?: string;
    kind: string;
    itemList?: NavItem[];
  }

  interface NavItem {
    title: string;
    path: string;
    icon: ReactElement;
  }

  const navDiv = (div: NavDiv, index: number) => {
    let component;
    switch (div.kind) {
      case "divider":
        component = <Divider key={index} />;
        break;
      case "subheader":
        component = (
          <List
            key={index}
            component="nav"
            aria-labelledby="nested-list-subheader"
            subheader={div.title && subheader(div.title)}
            sx={{
              background: "transparent",
              color: "inherit",
              fontFamily: "Mukta, Montserrat, sans-serif",
              fontWeight: "bold",
              fontSize: "18px",
            }}
          >
            {div.itemList &&
              div.itemList.map((item, index) =>
                listItem(
                  item.title,
                  item.path,
                  item.icon,
                  `${item.title}-${index}`,
                ),
              )}
          </List>
        );
    }
    return component;
  };

  const DrawerList = (
    <Box sx={{ overflow: "auto", marginTop: "5px" }}>
      {navigationList.map((item, index) => navDiv(item, index))}
    </Box>
  );

  //// RENDER

  return (
    <Box
      sx={{
        // use `dvh` for dynamic viewport height to handle mobile browser weirdness
        // but fallback to `vh` for browsers that don't support `dvh`
        // `&` is a workaround because sx prop can't have identical keys
        "&": {
          height: "100dvh",
        },
        height: "100vh",
        display: "flex",
        flexDirection: "column",
      }}
    >
      <TopNav />
      <Box sx={{ flexGrow: 1, display: "flex", width: "100vw" }}>
        {lgUp && (
          <Drawer
            anchor="left"
            open
            sx={{ width: drawerWidth }}
            PaperProps={{
              sx: {
                width: drawerWidth,
                backgroundColor: "background.default",
                color: "base.contrastText",
              },
            }}
            variant="permanent"
          >
            <Stack sx={{ height: "100%" }}>
              <Stack
                alignItems="center"
                direction="row"
                spacing={2}
                sx={{ padding: "4px 36px" }}
              >
                <Brand />
              </Stack>
              <Stack
                component="nav"
                spacing={2}
                sx={{
                  flexGrow: 1,
                  px: 2,
                }}
              >
                {DrawerList}
              </Stack>
            </Stack>
          </Drawer>
        )}

        <Box sx={{ width: "100%", padding: 0, margin: 0 }}>{children}</Box>
      </Box>
      <PlayBar />
    </Box>
  );
}

export function getLeftNavLayout(page: ReactElement) {
  return (
    <MasterDataLayout>
      <LayoutContext.Provider value="leftNav">
        <LeftNavLayout>{page}</LeftNavLayout>
      </LayoutContext.Provider>
    </MasterDataLayout>
  );
}
