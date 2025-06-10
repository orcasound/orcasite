import CloseIcon from "@mui/icons-material/Close";
import { Box, IconButton, Theme, useMediaQuery } from "@mui/material";
import dynamic from "next/dynamic";
import React, { useMemo } from "react";

import PlayerTimeDisplay from "@/components/CandidateList/PlayerTimeDisplay";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { useComputedPlaybackFields } from "@/hooks/beta/useComputedPlaybackFields";

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
  const {
    nowPlayingFeed,
    nowPlayingCandidate,
    setNowPlayingCandidate,
    setNowPlayingFeed,
  } = useNowPlaying();
  const { feeds } = useData();
  const { startOffset } = useComputedPlaybackFields(nowPlayingCandidate);

  const feed = useMemo(() => {
    if (nowPlayingCandidate) {
      const canFeed = feeds.find((f) => f.id === nowPlayingCandidate.feedId);
      if (canFeed) {
        return canFeed;
      } else {
        return null;
      }
    } else if (nowPlayingFeed) {
      return nowPlayingFeed;
    } else {
      return null;
    }
  }, [feeds, nowPlayingCandidate, nowPlayingFeed]);

  return (
    <Box className={"map-wrapper"} sx={{ flexGrow: 1, position: "relative" }}>
      <Box
        className="map-title"
        sx={{
          position: "absolute",
          top: smDown ? 21 : 16,
          left: 16,
          width: smDown ? "250px" : "300px",
          zIndex: (theme) => theme.zIndex.fab,
          display: "flex",
          flexDirection: "column",
          gap: "4px",
        }}
      >
        {/* {smDown && (
          <Box
            sx={{
              width: "50%",
              backgroundColor: "background.default",
              borderRadius: "4px",
            }}
          >
          </Box>
        )} */}
        {nowPlayingCandidate && (
          <PlayerTimeDisplay
            masterPlayerTimeRef={masterPlayerTimeRef}
            startOffset={startOffset} // needs to be passed in, don't recalculate the startOffset hook on every change
          />
        )}
      </Box>

      {nowPlayingCandidate && (
        <IconButton
          aria-label="close"
          className="candidate-map-close"
          onClick={() => {
            setNowPlayingFeed(feed);
            setNowPlayingCandidate(null);
          }}
          sx={{
            position: "absolute",
            right: "1rem",
            top: "1rem",
            color: (theme) => theme.palette.grey[500],
            background: (theme) => theme.palette.background.default,
            zIndex: (theme) => theme.zIndex.drawer + 1,
          }}
        >
          <CloseIcon />
        </IconButton>
      )}
      <MapWithNoSSR />
    </Box>
  );
}
