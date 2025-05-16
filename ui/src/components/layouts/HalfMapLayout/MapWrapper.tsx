import { Box, Theme, useMediaQuery } from "@mui/material";
import { type Map as LeafletMap } from "leaflet";
import dynamic from "next/dynamic";
import React, { useEffect, useMemo, useState } from "react";

import PlayerTimeDisplay from "@/components/CandidateList/PlayerTimeDisplay";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { useComputedPlaybackFields } from "@/hooks/useComputedPlaybackFields";

const MapWithNoSSR = dynamic(
  () => import("@/components/layouts/HalfMapLayout/NewMap"),
  {
    ssr: false,
  },
);

export function MapWrapper({
  masterPlayerTimeRef,
}: {
  masterPlayerTimeRef: React.MutableRefObject<number>;
}) {
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const { nowPlayingCandidate } = useNowPlaying();
  const { feeds } = useData();

  const CandidateFeed = useMemo(() => {
    if (!nowPlayingCandidate) return undefined;
    return feeds.find((feed) => feed.id === nowPlayingCandidate.feedId);
  }, [nowPlayingCandidate, feeds]);

  const { startOffset } = useComputedPlaybackFields(nowPlayingCandidate);
  const [map, setMap] = useState<LeafletMap>();

  useEffect(() => {
    if (CandidateFeed) {
      map?.setZoom(12);
      map?.panTo(CandidateFeed.latLng);
    } else {
      map?.setZoom(8);
    }
  }, [map, CandidateFeed]);

  return (
    <Box className={"map-wrapper"} sx={{ flexGrow: 1, position: "relative" }}>
      <Box
        className="map-title"
        sx={{
          position: "absolute",
          top: 16,
          left: 16,
          width: smDown ? "250px" : "300px",
          zIndex: 10000,
          display: "flex",
          flexDirection: "column",
          gap: "4px",
        }}
      >
        {nowPlayingCandidate && (
          <PlayerTimeDisplay
            nowPlaying={nowPlayingCandidate}
            masterPlayerTimeRef={masterPlayerTimeRef}
            startOffset={startOffset}
          />
        )}
      </Box>
      <MapWithNoSSR setMap={setMap} currentFeed={CandidateFeed} feeds={feeds} />
    </Box>
  );
}
