import { ExpandLess, Fullscreen, List, Map } from "@mui/icons-material";
import { Box, IconButton } from "@mui/material";
import { QueryClient } from "@tanstack/react-query";
import type { Map as LeafletMap } from "leaflet";
import dynamic from "next/dynamic";
import { useRouter } from "next/router";
import { ReactElement, ReactNode, useEffect, useState } from "react";

import Drawer from "@/components/Drawer";
import Header from "@/components/Header";
import {
  useDetectionsQuery,
  useFeedQuery,
  useFeedsQuery,
} from "@/graphql/generated";
import { useSightings } from "@/hooks/useSightings";
import { displayDesktopOnly, displayMobileOnly } from "@/styles/responsive";

import Player, { PlayerSpacer } from "../Player";

const MapWithNoSSR = dynamic(() => import("../Map"), {
  ssr: false,
});

const DEFAULT_CENTER: [number, number] = [48.1, -122.75];
const DEFAULT_ZOOM = 8;
const FEED_ZOOM = 12;

const feedFromSlug = (feedSlug: string) => ({
  id: feedSlug,
  name: feedSlug,
  slug: feedSlug,
  nodeName: feedSlug,
  // TODO: pass in bucket from dynamic feed instead of env/hardcoding
  bucket: process.env.NEXT_PUBLIC_S3_BUCKET ?? "audio-orcasound-net",
  // TODO: figure out which coordinates to use for dynamic feeds
  latLng: { lat: 47.6, lng: -122.3 },
});

function MapLayout({ children }: { children: ReactNode }) {
  const [drawerOpen, setDrawerOpen] = useState(true);
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
  const [map, setMap] = useState<LeafletMap | undefined>();
  const feeds = useFeedsQuery().data?.feeds ?? [];

  // Added: data call
  const sightings = useSightings().data?.results;
  const detections = useDetectionsQuery().data?.detections?.results;

  // End: sightings data call

  // update the currentFeed only if there's a new feed
  useEffect(() => {
    if (feed && feed.slug !== currentFeed?.slug) {
      setCurrentFeed(feed);
    }
  }, [feed, currentFeed]);

  // update map zoom / center based on feed in url, separately from currentFeed, so that map returns to default view but UI still reflects most-recently selected feed in player
  useEffect(() => {
    if (!map) return;

    // hot-reload safety guard to prevent calling map.setView on a stale Leaflet instance
    const mapWithPane = map as LeafletMap & { _mapPane?: unknown };
    if (!mapWithPane._mapPane) return;

    // Keep current viewport while route feed slug exists but feed query is still resolving to avoid jarring resets between page routes.
    if (slug && !feed) return;

    if (feed) {
      map.setView([feed.latLng.lat, feed.latLng.lng], FEED_ZOOM);
      return;
    }

    map.setView(DEFAULT_CENTER, DEFAULT_ZOOM);
  }, [map, feed, slug]);

  const invalidateSize = () => {
    if (map) {
      // wait 200ms before resizing so that drawer transition animations have a chance to finish
      // TODO: trigger resize directly from after transition instead of dead reckoning
      setTimeout(() => {
        map.invalidateSize({ pan: false });
      }, 200);
    }
  };

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
      <Header
        onBrandClick={() => {
          setDrawerOpen(true);
        }}
      />
      <Box sx={{ flexGrow: 1, display: "flex" }}>
        <Drawer
          setOpen={setDrawerOpen}
          open={drawerOpen}
          onClose={invalidateSize}
          onOpen={invalidateSize}
        >
          {children}
        </Drawer>
        <Box
          sx={{
            flexGrow: 1,
            display: "flex",
            flexDirection: "column",
            minWidth: 0,
            position: "relative",
          }}
        >
          <Box sx={{ flexGrow: 1 }}>
            <MapWithNoSSR
              setMap={(nextMap) => {
                // hot-reload safety guard to prevent setting map to a stale Leaflet instance
                setMap(nextMap ?? undefined);
              }}
              currentFeed={currentFeed}
              feeds={feeds}
              sightings={sightings}
              detections={detections}
            />
          </Box>
          <ToggleDrawerButton
            drawerOpen={drawerOpen}
            onClick={() => {
              setDrawerOpen(!drawerOpen);
              invalidateSize();
            }}
          />
          <PlayerSpacer sx={displayMobileOnly} />
          <Player currentFeed={currentFeed} />
        </Box>
      </Box>
    </Box>
  );
}

function ToggleDrawerButton({
  drawerOpen,
  onClick,
}: {
  drawerOpen: boolean;
  onClick: () => void;
}) {
  return (
    <>
      {/* Mobile */}
      <Box
        sx={{
          ...displayMobileOnly,
          position: { xs: "absolute" },
          right: { xs: 15 },
          bottom: { xs: 103 },
          zIndex: (theme) => theme.zIndex.drawer + 1,
        }}
      >
        <IconButton
          sx={{
            backgroundColor: (theme) =>
              drawerOpen ? theme.palette.primary.main : "white",
            color: (theme) =>
              drawerOpen ? "white" : theme.palette.primary.main,
            "&:hover": (theme) =>
              drawerOpen
                ? { background: theme.palette.primary.main, opacity: 0.8 }
                : { background: "white", opacity: 0.8 },
          }}
          title={drawerOpen ? "Show map" : "Expand details"}
          onClick={onClick}
        >
          {!drawerOpen && <ExpandLess />}
          {drawerOpen && <Map />}
        </IconButton>
      </Box>

      {/* Desktop */}
      <Box
        sx={{
          ...displayDesktopOnly,
          position: { sm: "absolute" },
          left: { sm: 15 },
          top: { sm: 15 },
          zIndex: (theme) => theme.zIndex.drawer - 1,
        }}
      >
        <IconButton
          sx={{
            backgroundColor: "white",
            "&:hover": { background: "white", opacity: 0.8 },
          }}
          title={drawerOpen ? "Full screen map" : "Expand details"}
          onClick={onClick}
        >
          {!drawerOpen && <List />}
          {drawerOpen && <Fullscreen />}
        </IconButton>
      </Box>
    </>
  );
}

export function getMapLayout(page: ReactElement) {
  return <MapLayout>{page}</MapLayout>;
}

export async function getMapStaticProps(queryClient: QueryClient) {
  await queryClient.prefetchQuery({
    queryKey: useFeedsQuery.getKey(),
    queryFn: useFeedsQuery.fetcher(),
  });
}
