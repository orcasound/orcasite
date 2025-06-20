import CloseIcon from "@mui/icons-material/Close";
import { Box, IconButton, Stack, Theme, useMediaQuery } from "@mui/material";
import dynamic from "next/dynamic";
import { useRouter } from "next/router";
import React, { useMemo } from "react";

import { timeRangeSelect } from "@/components/CandidateList/CandidateListFilters";
import PlayerTimeDisplay from "@/components/CandidateList/PlayerTimeDisplay";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { useComputedPlaybackFields } from "@/hooks/beta/useComputedPlaybackFields";
import formatDuration from "@/utils/masterDataHelpers";

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
    nowPlayingCandidate,
    nowPlayingFeed,
    setNowPlayingCandidate,
    setNowPlayingFeed,
  } = useNowPlaying();
  const { filters, feeds } = useData();
  const router = useRouter();
  const { startOffset } = useComputedPlaybackFields(nowPlayingCandidate);

  const candidateStart = nowPlayingCandidate?.startTimestamp ?? "";
  const currentTimeSeconds = new Date().getTime() / 1000;
  const timestampSeconds = new Date(candidateStart).getTime() / 1000;
  const timeAgoString = formatDuration(timestampSeconds, currentTimeSeconds);

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
          top: 16,
          left: 16,
          // width: smDown ? "250px" : "300px",
          zIndex: (theme) => theme.zIndex.fab,
          display: "flex",
          flexDirection: "column",
          alignItems: "flex-start",
          gap: "4px",
        }}
      >
        <Stack direction="row" gap=".5rem">
          <Box
            sx={{
              width: "auto",
              backgroundColor: "background.default",
              marginBottom: "6px",
              padding: "6px 12px",
              borderRadius: "4px",
              color: "primary.main",
            }}
          >
            {nowPlayingCandidate
              ? timeAgoString + " ago"
              : timeRangeSelect.find((el) => el.value === filters.timeRange)
                  ?.label}
          </Box>
        </Stack>
        {nowPlayingCandidate && (
          <PlayerTimeDisplay
            masterPlayerTimeRef={masterPlayerTimeRef}
            startOffset={startOffset} // needs to be passed in, don't recalculate the startOffset hook on every change
          />
        )}
      </Box>

      {smDown && nowPlayingCandidate && (
        <IconButton
          aria-label="close"
          className="candidate-map-close"
          onClick={() => {
            // router.back();
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
            "&:hover": {
              background: (theme) => theme.palette.background.default,
            },
          }}
        >
          <CloseIcon />
        </IconButton>
      )}
      <MapWithNoSSR key={router.pathname} />
    </Box>
  );
}
