import { PlayLessonOutlined } from "@mui/icons-material";
import BarChartIcon from "@mui/icons-material/BarChart";
import DataObjectIcon from "@mui/icons-material/DataObject";
import EarbudsIcon from "@mui/icons-material/Earbuds";
import MicIcon from "@mui/icons-material/Mic";
import PlayCircleOutlineIcon from "@mui/icons-material/PlayCircleOutline";
import { Box } from "@mui/material";
import Divider from "@mui/material/Divider";
import Drawer from "@mui/material/Drawer";
import List from "@mui/material/List";
import ListItem from "@mui/material/ListItem";
import ListItemButton from "@mui/material/ListItemButton";
import ListItemIcon from "@mui/material/ListItemIcon";
import ListItemText from "@mui/material/ListItemText";
import ListSubheader from "@mui/material/ListSubheader";
import Toolbar from "@mui/material/Toolbar";
import { useQuery } from "@tanstack/react-query";
import * as React from "react";
import { ReactElement, useMemo, useState } from "react";

import Header from "@/components/Header";
import Link from "@/components/Link";
import { DataProvider } from "@/context/DataContext";
import { useDetectionsQuery, useFeedsQuery } from "@/graphql/generated";
import { Candidate } from "@/pages/moderator/candidates";
import { AIData } from "@/types/DataTypes";

import PlayBar from "../PlayBar";

const drawerWidth = 240;

const navigation = [
  {
    kind: "subheader",
    title: "",
    children: [
      {
        title: "Candidates",
        path: "/moderator/candidates",
        icon: <PlayCircleOutlineIcon />,
      },
      {
        title: "Hydrophones",
        path: "/moderator/hydrophones/",
        icon: <MicIcon />,
      },
      {
        title: "Bouts",
        path: "/moderator/bouts/",
        icon: <EarbudsIcon />,
      },
      {
        title: "Learn",
        path: "/moderator/learn/",
        icon: <PlayLessonOutlined />,
      },
      {
        title: "Reports",
        path: "/moderator/reports",
        icon: <BarChartIcon />,
      },
      {
        title: "JSON",
        path: "/moderator/json",
        icon: <DataObjectIcon />,
      },
    ],
  },
  //  {    kind: "divider",  },
];

const endpointOrcahello =
  process.env.NEXT_PUBLIC_ORCAHELLO_API_URL ||
  "https://aifororcasdetections.azurewebsites.net/api/detections";
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
  return endpoint + "?" + params;
}
const standardizeFeedName = (name: string) => {
  switch (name) {
    case "Beach Camp at Sunset Bay":
      return "Sunset Bay";
    case "North SJC":
      return "North San Juan Channel";
    case "Haro Strait":
      return "Orcasound Lab";
    default:
      return name;
  }
};
const lookupFeedName = (
  id: string,
  feedList: { id: string; name: string }[],
) => {
  const feed = feedList.find((feed) => id === feed.id);
  return standardizeFeedName(feed ? feed.name : "feed not found");
};

function ModeratorLayout({ children }: { children: React.ReactNode }) {
  //// DATA

  const [nowPlaying, setNowPlaying] = useState({} as Candidate);

  // get data on hydrophones
  const feedsQueryResult = useFeedsQuery();
  const feedsData = useMemo(() => {
    const feeds = feedsQueryResult.data?.feeds ?? [];
    return feeds;
  }, [feedsQueryResult.data?.feeds]);

  // get data on human detections
  const detectionQueryResult = useDetectionsQuery();
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
  const baseDataset = useMemo(() => {
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
    };
  }, [datasetHuman, aiDetections, feedsData, isSuccess]);

  const dataset = useMemo(() => {
    return {
      ...baseDataset,
      nowPlaying,
      setNowPlaying,
    };
  }, [baseDataset, nowPlaying]);

  //// COMPONENTS

  const listItem = (title: string, path: string, icon: ReactElement) => (
    <Link
      key={title}
      href={path}
      underline="none"
      width={0.9}
      sx={{ color: "rgba(0,0,0,0.6)" }}
    >
      <ListItem disablePadding>
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

  const navDiv = (div: NavDiv, index: number) => {
    let component;
    switch (div.kind) {
      case "divider":
        component = <Divider />;
        break;
      case "subheader":
        component = (
          <List
            key={index}
            component="nav"
            aria-labelledby="nested-list-subheader"
            subheader={div.title && subheader(div.title)}
          >
            {div.children?.map((item) =>
              listItem(item.title, item.path, item.icon),
            )}
          </List>
        );
    }
    return component;
  };

  const DrawerList = (
    <Box sx={{ overflow: "auto", marginTop: "5px" }}>
      {navigation.map((item, index) => navDiv(item, index))}
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
      <Header />

      <Box sx={{ flexGrow: 1, display: "flex" }}>
        <div key={"right"}>
          <Drawer
            variant="permanent"
            sx={{
              width: drawerWidth,
              flexShrink: 0,
              [`& .MuiDrawer-paper`]: {
                width: drawerWidth,
                boxSizing: "border-box",
              },
            }}
          >
            <Toolbar />
            {DrawerList}
          </Drawer>
        </div>

        <Box
          // maxWidth="xl"
          sx={{ width: "100%", padding: 0, paddingLeft: 4, margin: 0 }}
        >
          <DataProvider data={dataset}>{children}</DataProvider>
        </Box>

        <PlayBar candidate={nowPlaying} />
      </Box>
    </Box>
  );
}

export function getModeratorLayout(page: ReactElement) {
  return <ModeratorLayout>{page}</ModeratorLayout>;
}
