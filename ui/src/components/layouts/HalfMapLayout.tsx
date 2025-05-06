import { Earbuds, Mic } from "@mui/icons-material";
import {
  BottomNavigation,
  BottomNavigationAction,
  Box,
  Theme,
  useMediaQuery,
} from "@mui/material";
import type { Map as LeafletMap } from "leaflet";
import dynamic from "next/dynamic";
import { useRouter } from "next/router";
import { ReactElement, ReactNode, useEffect, useState } from "react";

import Header from "@/components/Header";
import { LayoutContext } from "@/context/LayoutContext";
import { useFeedQuery, useFeedsQuery } from "@/graphql/generated";

import CandidatesTabs from "../CandidateList/CandidatesTabs";
import PlayBar from "../PlayBar";
import { MasterDataLayout } from "./MasterDataLayout";

const MapWithNoSSR = dynamic(() => import("@/components/Map"), {
  ssr: false,
});

const feedFromSlug = (feedSlug: string) => ({
  id: feedSlug,
  name: feedSlug,
  slug: feedSlug,
  nodeName: feedSlug,
  // TODO: pass in bucket from dynamic feed instead of env/hardcoding
  bucket: process.env.NEXT_PUBLIC_S3_BUCKET ?? "audio-orcasound-net",
  // TODO: figure out which coordinates to use for dynamic feeds
  latLng: { lat: 48.6, lng: -122.3 },
});

function HalfMapLayout({ children }: { children: ReactNode }) {
  const router = useRouter();
  const slug = router.query.feed as string;
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));

  const [menuTab, setMenuTab] = useState(0);
  const menu = (
    <BottomNavigation
      showLabels
      value={menuTab}
      onChange={(event, newMenuTab) => {
        setMenuTab(newMenuTab);
      }}
      sx={{ height: "69px" }}
    >
      <BottomNavigationAction label="Listen Live" icon={<Mic />} />
      <BottomNavigationAction label="Recordings" icon={<Earbuds />} />
    </BottomNavigation>
  );

  const isDynamic = router.asPath.split("/")[1] === "dynamic";
  // don't make feed request if there's no feed slug or is dynamic
  const feedFromQuery = useFeedQuery(
    { slug: slug },
    { enabled: !!slug || isDynamic },
  ).data?.feed;
  const feed = isDynamic ? feedFromSlug(slug) : feedFromQuery;

  const [currentFeed, setCurrentFeed] = useState(feed);
  const [map, setMap] = useState<LeafletMap>();
  const feeds = useFeedsQuery().data?.feeds ?? [];
  const firstOnlineFeed = feeds.filter(({ online }) => online)[0];

  // update the currentFeed only if there's a new feed
  useEffect(() => {
    if (feed && feed.slug !== currentFeed?.slug) {
      setCurrentFeed(feed);
      map?.setZoom(4);
      map?.panTo(feed.latLng);
    }
    if (!feed && !currentFeed && firstOnlineFeed) {
      setCurrentFeed(firstOnlineFeed);
    }
  }, [feed, map, currentFeed, firstOnlineFeed]);

  const mapBox = (
    <Box sx={{ flexGrow: 1 }}>
      <MapWithNoSSR setMap={setMap} currentFeed={currentFeed} feeds={feeds} />
    </Box>
  );

  return (
    <Box
      sx={{
        // use `dvh` for dynamic viewport height to handle mobile browser weirdness
        // but fallback to `vh` for browsers that don't support `dvh`
        // `&` is a workaround because sx prop can't have identical keys
        // "&": {
        //   height: "100dvh",
        // },
        height: "100vh",
        display: "flex",
        flexDirection: "column",
        flexGrow: 1,
      }}
    >
      <Header />
      <Box
        component="main"
        sx={{
          display: "flex",
          flex: 1,
          overflow: "hidden",
          position: "relative",
        }}
      >
        {!mdDown && (
          <Box
            className="side-list"
            sx={{
              borderRightColor: "divider",
              borderRightStyle: "solid",
              borderRightWidth: 1,
              width: "45%",
              overflow: "auto",
            }}
          >
            <CandidatesTabs mapBox={mapBox} />
          </Box>
        )}

        <Box
          sx={{
            flex: 1,
            display: "flex",
            flexDirection: "column",
            minWidth: 0,
            position: "relative",
          }}
        >
          {!mdDown && router.query.candidateId === undefined ? (
            mapBox
          ) : mdDown &&
            router.query.candidateId === undefined &&
            menuTab === 0 ? (
            <CandidatesTabs mapBox={mapBox} tab={"Listen Live"} />
          ) : mdDown &&
            router.query.candidateId === undefined &&
            menuTab === 1 ? (
            <Box
              sx={{
                width: "100%",
                overflow: "auto",
              }}
            >
              <CandidatesTabs mapBox={mapBox} tab={"Recordings"} />
            </Box>
          ) : (
            children
          )}
        </Box>
      </Box>
      <PlayBar menu={menu} />
    </Box>
  );
}

export function getHalfMapLayout(page: ReactElement) {
  return (
    <MasterDataLayout>
      <LayoutContext.Provider value="halfMap">
        <HalfMapLayout>{page}</HalfMapLayout>
      </LayoutContext.Provider>
    </MasterDataLayout>
  );
}
