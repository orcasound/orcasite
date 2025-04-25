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
import { useQuery } from "@tanstack/react-query";
import { gql, request } from "graphql-request";
import Image from "next/image";
import * as React from "react";
import { ReactElement, useEffect, useMemo, useState } from "react";

import Link from "@/components/Link";
import { DataProvider } from "@/context/DataContext";
import {
  Detection,
  Feed,
  useDetectionsQuery,
  useFeedsQuery,
} from "@/graphql/generated";
import wordmark from "@/public/wordmark/wordmark-white.svg";
import { Candidate } from "@/types/DataTypes";
import { AIData } from "@/types/DataTypes";
import { analytics } from "@/utils/analytics";

import { TopNav } from "./Devias-dashboard/vertical-layout/top-nav";
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
const lookupFeedName = (id: string, feedList: Feed[]) => {
  let name = "feed name not found";
  feedList.forEach((feed) => {
    if (id === feed.id) {
      name = feed.name;
    }
  });
  return standardizeFeedName(name);
};

const lookupFeedId = (name: string, feedList: Feed[]) => {
  let id = "feed id not found";
  const standardizedName = standardizeFeedName(name);
  feedList.forEach((feed) => {
    if (standardizedName === feed.name) {
      id = feed.id;
    }
  });
  return id;
};

export function ModeratorLayout({ children }: { children: React.ReactNode }) {
  //// DATA
  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  const [nowPlaying, setNowPlaying] = useState({} as Candidate);

  // get data on hydrophones from seed data
  const feedsQueryResult = useFeedsQuery();
  const feedsData = useMemo(() => {
    return feedsQueryResult.data?.feeds ?? [];
  }, [feedsQueryResult.data?.feeds]);

  // get data on human detections using endpoint directly (gives live data in dev)
  const endpoint = "https://live.orcasound.net/graphiql/";
  const detectionsGQL = gql`
    {
      detections(limit: 250) {
        results {
          id
          feedId
          listenerCount
          category
          description
          playerOffset
          playlistTimestamp
          timestamp
          candidate {
            id
            feedId
          }
          feed {
            name
            id
          }
        }
      }
    }
  `;
  const feedsGQL = gql`
    {
      feeds {
        id
        name
        slug
        nodeName
        latLng {
          lat
          lng
        }
        imageUrl
        thumbUrl
        mapUrl
        bucket
        online
      }
    }
  `;
  // running this without using live data endpoint to see if performance improves
  const [useLiveData, setUseLiveData] = useState(true);
  type LiveDataResponse = {
    detections: {
      results: Detection[];
    };
  };
  const fetchLiveDetections = (): Promise<LiveDataResponse> =>
    request(endpoint, detectionsGQL);
  const {
    data: liveDetectionData,
    // isLoading: isLoadingLive,
    // error: errorLive,
  } = useQuery({
    queryKey: ["detections-live"],
    queryFn: fetchLiveDetections,
    enabled: useLiveData,
  });
  type LiveFeedsResponse = {
    feeds: Feed[];
  };
  const fetchLiveFeeds = (): Promise<LiveFeedsResponse> =>
    request(endpoint, feedsGQL);
  const {
    data: liveFeedsData,
    // isLoading: isLoadingFeedsLive,
    // error: errorFeedsLive,
  } = useQuery({
    queryKey: ["feeds-live"],
    queryFn: fetchLiveFeeds,
    enabled: useLiveData,
  });

  // get data on human detections using graphql/generated (gives seed data in dev)
  const detectionQueryResult = useDetectionsQuery();

  // populate data based on live/seed toggle
  const humanDetections = useLiveData
    ? (liveDetectionData?.detections?.results ?? [])
    : ((detectionQueryResult.data?.detections?.results ?? []) as Detection[]);

  const feedList = useMemo(() => {
    return useLiveData
      ? (liveFeedsData?.feeds ?? [])
      : ((feedsData ?? []) as Feed[]);
  }, [useLiveData, liveFeedsData, feedsData]);

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

  // // LocalStorage key name
  const AI_DETECTIONS_CACHE_KEY = "orcahello-ai-detections";

  const [cachedAIDetections, setCachedAIDetections] = useState<AIData[] | null>(
    null,
  );

  useEffect(() => {
    if (typeof window !== "undefined") {
      const raw = localStorage.getItem(AI_DETECTIONS_CACHE_KEY);
      if (raw) {
        try {
          setCachedAIDetections(JSON.parse(raw));
        } catch (e) {
          console.log("Failed to parse cached AI detections", e);
        }
      }
    }
  }, []);

  useEffect(() => {
    if (isSuccess && data) {
      try {
        localStorage.setItem(AI_DETECTIONS_CACHE_KEY, JSON.stringify(data));
      } catch (e) {
        console.log("Failed to save AI detections to localStorage", e);
      }
    }
  }, [isSuccess, data]);

  const aiDetections = data ?? cachedAIDetections;

  // deduplicate data on human detections
  const dedupeHuman = humanDetections.filter(
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
    hydrophone: lookupFeedName(el.feedId!, feedList),
    comments: el.description,
    newCategory: el.category!,
    timestampString: el.timestamp.toString(),
  }));

  // combine global data into one object, to be passed into Data Provider for all child pages
  const dataset = useMemo(() => {
    const truePositives = aiDetections?.filter(
      (el: AIData) => el.found === "yes",
    );
    const datasetAI =
      truePositives?.map((el: AIData) => ({
        ...el,
        type: "ai",
        hydrophone: standardizeFeedName(el.location.name),
        feedId: lookupFeedId(el.location.name, feedList),
        newCategory: "WHALE (AI)",
        timestampString: el.timestamp.toString(),
      })) ?? [];
    return {
      human: datasetHuman,
      ai: datasetAI,
      combined: [...datasetHuman, ...datasetAI],
      feeds: feedList,
      isSuccess: isSuccess,
      nowPlaying: nowPlaying,
      setNowPlaying: setNowPlaying,
    };
  }, [datasetHuman, aiDetections, feedList, isSuccess, nowPlaying]);

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
      {/* <Header /> */}
      {process.env.NODE_ENV === "development" && (
        <button
          onClick={() => setUseLiveData((prev) => !prev)}
          style={{
            position: "fixed",
            zIndex: 10000,
            bottom: "4%",
            right: "5%",
            background: "yellow",
          }}
        >
          {useLiveData ? "Using LIVE data" : "Using SEED data"}
        </button>
      )}
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
        )}

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
