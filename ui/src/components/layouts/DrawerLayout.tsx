import { PlayLessonOutlined } from "@mui/icons-material";
import BarChartIcon from "@mui/icons-material/BarChart";
import DataObjectIcon from "@mui/icons-material/DataObject";
import EarbudsIcon from "@mui/icons-material/Earbuds";
import GraphicEqIcon from "@mui/icons-material/GraphicEq";
import { Box, Container } from "@mui/material";
import Divider from "@mui/material/Divider";
import List from "@mui/material/List";
import ListItem from "@mui/material/ListItem";
import ListItemButton from "@mui/material/ListItemButton";
import ListItemIcon from "@mui/material/ListItemIcon";
import ListItemText from "@mui/material/ListItemText";
import ListSubheader from "@mui/material/ListSubheader";
import Toolbar from "@mui/material/Toolbar";
import * as React from "react";
import { ReactElement } from "react";

import Drawer from "@/components/Drawer";
import Header from "@/components/Header";
import Link from "@/components/Link";

const navigation = [
  {
    kind: "subheader",
    title: "New versions",
    children: [
      {
        title: "Reports",
        path: "/moderator/",
        icon: <BarChartIcon />,
      },
      {
        title: "Hydrophones",
        path: "/moderator/listen/",
        icon: <GraphicEqIcon />,
      },
      {
        title: "Archive",
        path: "/moderator/learn/",
        icon: <EarbudsIcon />,
      },
      {
        title: "JSON",
        path: "/moderator/json",
        icon: <DataObjectIcon />,
      },
    ],
  },
  {
    kind: "divider",
  },
  {
    kind: "subheader",
    title: "Existing versions",
    children: [
      {
        title: "Reports",
        path: "/reports/",
        icon: <BarChartIcon />,
      },
      {
        title: "Bouts",
        path: "/bouts/",
        icon: <EarbudsIcon />,
      },
      {
        title: "Listen",
        path: "/listen/",
        icon: <GraphicEqIcon />,
      },
      {
        title: "Learn",
        path: "/moderator/learn/",
        icon: <PlayLessonOutlined />,
      },
    ],
  },
];

function ModeratorLayout({ children }: { children: React.ReactNode }) {
  const listItem = (title: string, path: string, icon: ReactElement) => (
    <Link
      href={path}
      underline="none"
      width={0.9}
      sx={{ color: "rgba(0,0,0,0.6)" }}
    >
      <ListItem key={title} disablePadding>
        <ListItemButton>
          <ListItemIcon>{icon}</ListItemIcon>
          <ListItemText primary={title} />
        </ListItemButton>
      </ListItem>
    </Link>
  );

  const subheader = (content: string) => (
    <ListSubheader component="div" id={content.replace(" ", "-").toLowerCase()}>
      {content}
    </ListSubheader>
  );

  interface NavDiv {
    title?: string;
    kind: string;
    children?: NavItem[];
  }

  interface NavItem {
    title: string;
    path: string;
    icon: ReactElement;
  }

  const navDiv = (div: NavDiv) => {
    let component;
    switch (div.kind) {
      case "divider":
        component = <Divider />;
        break;
      case "subheader":
        component = (
          <List
            component="nav"
            aria-labelledby="nested-list-subheader"
            subheader={div.title && subheader(div.title)}
          >
            {div.children &&
              div.children.map((item) =>
                listItem(item.title, item.path, item.icon),
              )}
          </List>
        );
    }
    return component;
  };

  const DrawerList = (
    <Box sx={{ overflow: "auto", marginTop: "5px" }}>
      {navigation.map((item) => navDiv(item))}
    </Box>
  );

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
      <Header />

      <Box sx={{ flexGrow: 1, display: "flex" }}>
        <div key={"right"}>
          <Drawer open={true} setOpen={() => {}}>
            <Toolbar />
            {DrawerList}
          </Drawer>
        </div>

        <Container maxWidth="xl">{children}</Container>
      </Box>
    </Box>
  );
}

export function getDrawerLayout(page: ReactElement) {
  return <ModeratorLayout>{page}</ModeratorLayout>;
}
