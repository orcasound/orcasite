import { Box, Theme, useMediaQuery } from "@mui/material";
import dynamic from "next/dynamic";
import React from "react";

import PlayerTimeDisplay from "@/components/CandidateList/PlayerTimeDisplay";
import { useNowPlaying } from "@/context/NowPlayingContext";

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

  return (
    <Box className={"map-wrapper"} sx={{ flexGrow: 1, position: "relative" }}>
      <Box
        className="map-title"
        sx={{
          position: "absolute",
          top: 16,
          left: 16,
          width: smDown ? "250px" : "300px",
          zIndex: (theme) => theme.zIndex.fab,
          display: "flex",
          flexDirection: "column",
          gap: "4px",
        }}
      >
        {nowPlayingCandidate && (
          <PlayerTimeDisplay masterPlayerTimeRef={masterPlayerTimeRef} />
        )}
      </Box>
      <MapWithNoSSR />
    </Box>
  );
}
