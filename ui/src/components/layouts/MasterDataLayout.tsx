import { Box, CssBaseline } from "@mui/material";
import { ThemeProvider } from "@mui/material";
import { useQuery } from "@tanstack/react-query";
import { gql, request } from "graphql-request";
import * as React from "react";
import { ReactElement, useEffect, useMemo, useState } from "react";

import { DataProvider } from "@/context/DataContext";
import { NowPlayingProvider } from "@/context/NowPlayingContext";
import {
  Detection,
  Feed,
  useDetectionsQuery,
  useFeedsQuery,
} from "@/graphql/generated";
import darkTheme from "@/styles/darkTheme";
import { AIData } from "@/types/DataTypes";

import PlayBar from "../PlayBar";

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
    case "North SJC":
      return "North San Juan Channel";
    case "Haro Strait":
      return "Orcasound Lab";
    default:
      return name;
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

export function MasterDataLayout({ children }: { children: React.ReactNode }) {
  //// DATA

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
    };
  }, [datasetHuman, aiDetections, feedList, isSuccess]);

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
      <NowPlayingProvider>
        <ThemeProvider theme={darkTheme}>
          <CssBaseline />
          <DataProvider data={dataset}>
            {children}
            <PlayBar />
          </DataProvider>
        </ThemeProvider>
      </NowPlayingProvider>
    </Box>
  );
}

export function getMasterDataLayout(page: ReactElement) {
  return <MasterDataLayout>{page}</MasterDataLayout>;
}
