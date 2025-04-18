import { Box, Stack, Typography } from "@mui/material";
import Divider from "@mui/material/Divider";
import Drawer from "@mui/material/Drawer";
import List from "@mui/material/List";
import ListItem from "@mui/material/ListItem";
import ListItemButton from "@mui/material/ListItemButton";
import ListItemIcon from "@mui/material/ListItemIcon";
import ListSubheader from "@mui/material/ListSubheader";
import { useQuery } from "@tanstack/react-query";
import Image from "next/image";
import * as React from "react";
import { ReactElement, useMemo, useState } from "react";

import Link from "@/components/Link";
import { DataProvider } from "@/context/DataContext";
import {
  DetectionCategory,
  useDetectionsQuery,
  useFeedsQuery,
} from "@/graphql/generated";
import { Candidate } from "@/pages/moderator/candidates";
import wordmark from "@/public/wordmark/wordmark-white.svg";
import { AIData } from "@/types/DataTypes";
import { analytics } from "@/utils/analytics";

import { TopNav } from "./Devias-dashboard/vertical-layout/top-nav";
import navigationHalfMap from "./navigationHalfMap";

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

const endpointOrcahello =
  "https://aifororcasdetections.azurewebsites.net/api/detections?";
const daysAgo = 7;
const paramsOrcahello = {
  page: 1,
  sortBy: "timestamp",
  sortOrder: "desc",
  timeframe: "all",
  dateFrom: new Date(new Date().setDate(new Date().getDate() - daysAgo))
    .toLocaleDateString()
    .replaceAll(/\//g, "%2F"),
  dateTo: new Date().toLocaleDateString().replaceAll(/\//g, "%2F"),
  location: "all",
  recordsPerPage: 100,
};
function constructUrl(endpoint: string, paramsObj: object) {
  let params = "";
  const entries = Object.entries(paramsObj);
  for (const [key, value] of entries) {
    const str = [key, value].join("=") + "&";
    params += str;
  }
  return endpoint + params;
}
const standardizeFeedName = (name: string) => {
  switch (name) {
    case "Beach Camp at Sunset Bay":
      return "Sunset Bay";
      break;
    case "North SJC":
      return "North San Juan Channel";
      break;
    case "Haro Strait":
      return "Orcasound Lab";
      break;
    default:
      return name;
      break;
  }
};
const lookupFeedName = (
  id: string,
  feedList: { id: string; name: string }[],
) => {
  let name = "feed not found";
  feedList.forEach((feed) => {
    if (id === feed.id) {
      name = feed.name;
    }
  });
  return standardizeFeedName(name);
};

export function ModeratorLayout({ children }: { children: React.ReactNode }) {
  //// DATA

  const [nowPlaying, setNowPlaying] = useState({} as Candidate);

  // get data on hydrophones
  const feedsQueryResult = useFeedsQuery();
  const feedsData = useMemo(() => {
    return feedsQueryResult.data?.feeds ?? [];
  }, [feedsQueryResult.data?.feeds]);

  type CategoryOptions = "WHALE" | "WHALE (AI)" | "VESSEL" | "OTHER" | "ALL";
  const [category, setCategory] = useState<CategoryOptions>("ALL");

  // get data on human detections
  const detectionQueryResult = useDetectionsQuery(
    ["WHALE", "VESSEL", "OTHER"].includes(category)
      ? { filter: { category: { eq: category as DetectionCategory } } }
      : {},
    { enabled: ["WHALE", "VESSEL", "OTHER", "ALL"].includes(category || "") },
  );
  const detectionsData = detectionQueryResult.data?.detections?.results ?? [];

  // get data on AI detections
  const fetchOrcahelloData = async () => {
    const response = await fetch(
      constructUrl(endpointOrcahello, paramsOrcahello),
    );
    if (!response.ok) {
      throw new Error("Network response from Orcahello was not ok");
    }
    return response.json();
  };

  const { data, isSuccess } = useQuery({
    queryKey: ["ai-detections"],
    queryFn: fetchOrcahelloData,
  });
  const aiDetections = data;

  // deduplicate data on human detections
  const dedupeHuman = detectionsData.filter(
    (obj, index, arr) =>
      arr.findIndex(
        (el) =>
          el.timestamp === obj.timestamp && el.description === obj.description,
      ) === index,
  );

  // standardize data from Orcasound and OrcaHello
  const datasetHuman = dedupeHuman.map((el) => ({
    ...el,
    type: "human",
    hydrophone: lookupFeedName(el.feedId!, feedsData),
    comments: el.description,
    newCategory: el!.category!,
    timestampString: el.timestamp.toString(),
  }));

  // combine global data into one object, to be passed into Data Provider for all child pages
  const dataset = useMemo(() => {
    const datasetAI =
      aiDetections?.map((el: AIData) => ({
        ...el,
        type: "ai",
        hydrophone: standardizeFeedName(el.location.name),
        newCategory: "WHALE (AI)",
        timestampString: el.timestamp.toString(),
      })) ?? [];
    return {
      human: datasetHuman,
      ai: datasetAI,
      combined: [...datasetHuman, ...datasetAI],
      feeds: feedsData,
      isSuccess: isSuccess,
      nowPlaying: nowPlaying,
      setNowPlaying: setNowPlaying,
    };
  }, [datasetHuman, aiDetections, feedsData, isSuccess, nowPlaying]);

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
          // marginBottom: "8px",
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
          {/* <ListItemText primary={title} style={{margin: 0, fontWeight: "inherit"}} /> */}
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
    children?: NavItem[];
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
            {div.children &&
              div.children.map((item, index) =>
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
      {navigationHalfMap.map((item, index) => navDiv(item, index))}
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
      {/* <Header /> */}
      <TopNav />
      <Box sx={{ flexGrow: 1, display: "flex", width: "100vw" }}>
        <Drawer
          anchor="left"
          open
          sx={{ width: drawerWidth }}
          PaperProps={{
            sx: {
              width: drawerWidth,
              backgroundColor: "base.main",
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

        {/* <div key={"left"} className="drawer-div">
          <Drawer
            PaperProps={{
              sx: {
                backgroundColor: "base.main",
                color: "base.contrastText",
              }
            }}
            variant="permanent"
            sx={{
              flexShrink: 0,
              width: drawerWidth,
              [`& .MuiDrawer-paper`]: {
                width: drawerWidth,
                boxSizing: "border-box",
              },
            }}
          >
          <Box 
            sx={{
              backgroundColor: "base.main",
              color: "base.contrastText",
              height: "100vh",
              // overflow: "scroll",
              padding: "0px 16px",
              fontWeight: "700"
            }}
          >
            <div style={{height: ".5rem"}} />
            <Toolbar />
            {DrawerList}
          </Box>
          </Drawer>
          </div> */}
        <Box
          // maxWidth="xl"
          sx={{ width: "100%", padding: 0, margin: 0 }}
        >
          <DataProvider data={dataset}>{children}</DataProvider>
        </Box>
        {/* <PlayBar candidate={nowPlaying} /> */}
      </Box>
    </Box>
  );
}

export function getModeratorLayout(page: ReactElement) {
  return <ModeratorLayout>{page}</ModeratorLayout>;
}
