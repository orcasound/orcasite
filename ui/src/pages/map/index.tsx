import { Box, Divider, Typography } from "@mui/material";
import type { Map as LeafletMap } from "leaflet";
import dynamic from "next/dynamic";
import { useRouter } from "next/router";
import { ReactNode, useEffect, useState } from "react";

import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import Player, { PlayerSpacer } from "@/components/Player";
import { useFeedQuery, useFeedsQuery } from "@/graphql/generated";
import { displayMobileOnly } from "@/styles/responsive";

import BoutsPage from "../bouts";

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

export default function MapPage({ _children }: { _children: ReactNode }) {
  const router = useRouter();
  const slug = router.query.feed as string;

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

  useEffect(() => {
    console.log("feed is " + feed);
    console.log("current feed is " + currentFeed);
    console.log("firstOnline feed is " + firstOnlineFeed);
  }, [feed, map, currentFeed, firstOnlineFeed]);

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
      <Divider />
      <Box
        component="main"
        sx={{
          display: "flex",
          flex: 1,
          overflow: "hidden",
          position: "relative",
        }}
      >
        <Box
          className="side-list"
          sx={{
            borderRightColor: "divider",
            borderRightStyle: "solid",
            borderRightWidth: 1,
            width: "45%",
            // flex: "0 0 400px"
          }}
        >
          <Box sx={{ p: 2 }}>
            <Typography variant="h5">Livestreams</Typography>
          </Box>

          {/* <FeedsPage /> */}
          {/* <Candidates /> */}
          <BoutsPage />
        </Box>

        <Box
          sx={{
            flex: 1,
            display: "flex",
            flexDirection: "column",
            minWidth: 0,
            position: "relative",
          }}
        >
          <Box sx={{ flexGrow: 1 }}>
            <MapWithNoSSR
              setMap={setMap}
              currentFeed={currentFeed}
              feeds={feeds}
            />
          </Box>

          <PlayerSpacer sx={displayMobileOnly} />
          <Player currentFeed={currentFeed} />
        </Box>
      </Box>
    </Box>
  );
}

MapPage.getLayout = getModeratorLayout;
