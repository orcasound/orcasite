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
import { AIData, Sighting } from "@/types/DataTypes";

const now = new Date();
const todayUTC = {
  yyyy: now.getUTCFullYear(),
  mm: String(now.getUTCMonth() + 1).padStart(2, "0"), // e.g. month as "05"
  dd: String(now.getUTCDate()).padStart(2, "0"),
};
const apiTodayUTC = `${todayUTC.yyyy}-${todayUTC.mm}-${todayUTC.dd}`;

const endpointOrcahello =
  "https://aifororcasdetections.azurewebsites.net/api/detections";
const startDateOrcahello = "2025-01-01";
const paramsOrcahello = {
  page: 1,
  sortBy: "timestamp",
  sortOrder: "desc",
  timeframe: "all",
  dateFrom: startDateOrcahello,
  dateTo: apiTodayUTC,
  location: "all",
  recordsPerPage: 100,
};

const endpointCascadia =
  "https://maplify.com/waseak/php/search-all-sightings.php";
const startDateCascadia = "2025-01-01";
const paramsCascadia = {
  BBOX: "-136,36,-120,54",
  start: startDateCascadia,
  end: apiTodayUTC,
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
    const feedName = standardizeFeedName(feed.name);
    if (standardizedName === feedName) {
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
  // ability to toggle between live API data vs seed data
  const [useLiveData, setUseLiveData] = useState(true);

  // live data call for Detections
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

  // live data call for Feeds
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

  const radius = 3;
  const addLat = radius / 69;
  const addLong = (lat: number) =>
    radius / (69 * Math.cos((lat * Math.PI) / 180));

  const feedCoordinates = feedList.map((feed) => ({
    name: feed.name,
    lat: feed.latLng.lat,
    lng: feed.latLng.lng,
    minLat: feed.latLng.lat - addLat,
    maxLat: feed.latLng.lat + addLat,
    minLng: feed.latLng.lng - addLong(feed.latLng.lat),
    maxLng: feed.latLng.lng + addLong(feed.latLng.lat),
  }));

  const assignSightingHydrophones = (sighting: Sighting) => {
    let hydrophone: string = "out of range";
    feedCoordinates.forEach((feed) => {
      const inLatRange =
        sighting.latitude >= feed.minLat && sighting.latitude <= feed.maxLat;
      const inLngRange =
        sighting.longitude >= feed.minLng && sighting.longitude <= feed.maxLng;
      if (inLatRange && inLngRange) {
        hydrophone = feed.name;
      }
    });
    hydrophone = standardizeFeedName(hydrophone);
    return hydrophone;
  };

  // get data on AI detections
  // TODO: provide a type for OrcahelloResponse

  const fetchOrcahelloData = async () => {
    const response = await fetch(
      constructUrl(endpointOrcahello, paramsOrcahello),
    );
    if (!response.ok) {
      throw new Error("Network response from Orcahello was not ok");
    }
    return response.json();
  };

  const { data: dataOrcahello, isSuccess: isSuccessOrcahello } = useQuery({
    queryKey: ["ai-detections"],
    queryFn: fetchOrcahelloData,
  });

  // // LocalStorage key name for AI detections
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
    if (isSuccessOrcahello && dataOrcahello) {
      try {
        localStorage.setItem(
          AI_DETECTIONS_CACHE_KEY,
          JSON.stringify(dataOrcahello),
        );
      } catch (e) {
        console.log("Failed to save AI detections to localStorage", e);
      }
    }
  }, [isSuccessOrcahello, dataOrcahello]);

  const aiDetections = dataOrcahello ?? cachedAIDetections;

  const fetchCascadiaData = async () => {
    const response = await fetch(
      constructUrl(endpointCascadia, paramsCascadia),
    );
    if (!response.ok) {
      throw new Error("Network response from Orcahello was not ok");
    }
    return response.json();
  };

  const { data: dataCascadia, isSuccess: isSuccessCascadia } = useQuery({
    queryKey: ["sightings"],
    queryFn: fetchCascadiaData,
  });

  // deduplicate data on human detections
  const dedupeHuman = humanDetections.filter(
    (obj, index, arr) =>
      arr.findIndex(
        (el) =>
          el.timestamp === obj.timestamp && el.description === obj.description,
      ) === index,
  );

  // standardize data from Orcasound and OrcaHello
  const datasetHuman = useMemo(() => {
    if (!feedList.length) return [];
    return dedupeHuman.map((el) => ({
      ...el,
      type: "human",
      hydrophone: lookupFeedName(el.feedId!, feedList),
      comments: el.description,
      newCategory: el.category!,
      timestampString: el.timestamp.toString(),
    }));
  }, [dedupeHuman, feedList]);

  // prepare Cascadia sightings data to join with Orcasound and Orcahello
  useEffect(() => {
    console.log("feeds: " + JSON.stringify(feedList, null, 2));
    // console.log("orcahello url: " + constructUrl(endpointOrcahello, paramsOrcahello))
    // console.log("cascadia url: " + constructUrl(endpointCascadia, paramsCascadia))
    // console.log("dataCascadia: " + JSON.stringify(dataCascadia))
    // console.log("dataOrcahello: " + JSON.stringify(dataOrcahello))
  });
  const datasetCascadia = useMemo(() => {
    if (!Array.isArray(dataCascadia?.results)) return [];
    return dataCascadia.results.map((el: Sighting) => ({
      ...el,
      type: "sightings",
      newCategory: "SIGHTINGS",
      hydrophone: assignSightingHydrophones(el),
      feedId: lookupFeedId(assignSightingHydrophones(el), feedList),
      timestampString: el.created,
      timestamp: new Date(el.created),
    }));
  }, [dataCascadia, feedList]);

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
        feedId: lookupFeedId(standardizeFeedName(el.location.name), feedList),
        newCategory: "WHALE (AI)",
        timestampString: el.timestamp.toString(),
      })) ?? [];
    return {
      human: datasetHuman,
      ai: datasetAI,
      sightings: datasetCascadia,
      combined: [...datasetHuman, ...datasetAI, ...datasetCascadia],
      feeds: feedList,
      isSuccessOrcahello: isSuccessOrcahello,
    };
  }, [
    datasetHuman,
    aiDetections,
    datasetCascadia,
    feedList,
    isSuccessOrcahello,
  ]);

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
          <DataProvider data={dataset}>{children}</DataProvider>
        </ThemeProvider>
      </NowPlayingProvider>
    </Box>
  );
}

export function getMasterDataLayout(page: ReactElement) {
  return <MasterDataLayout>{page}</MasterDataLayout>;
}
