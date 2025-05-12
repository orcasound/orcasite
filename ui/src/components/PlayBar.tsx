import {
  AppBar,
  Stack,
  Theme,
  Toolbar,
  Tooltip,
  useMediaQuery,
} from "@mui/material";
import React, { MutableRefObject, useMemo } from "react";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { formatTimestamp } from "@/utils/time";

import { useComputedPlaybackFields } from "./CandidateList/useComputedPlaybackFields";
import { PlaybarPlayer } from "./Player/PlaybarPlayer";

export default function PlayBar({
  mobileMenu,
  masterPlayerTimeRef,
}: {
  mobileMenu?: React.ReactNode;
  masterPlayerTimeRef?: MutableRefObject<number>;
}) {
  const { nowPlaying, setNowPlaying, queue } = useNowPlaying();
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  const detections = nowPlaying?.array;
  const hydrophone = nowPlaying?.hydrophone;
  const clipCount = nowPlaying?.clipCount;

  const { feeds } = useData();
  const feed = feeds.find((feed) => feed.id === detections?.[0]?.feedId);

  const { playlistTimestamp, playlistStartTime, startOffset, endOffset } =
    useComputedPlaybackFields(nowPlaying, feed?.id);

  // // skip the track if there is no audio -- this was causing strange rendering behavior
  // useEffect(() => {
  //   if(!nowPlaying) return
  //   const duration = endOffset - startOffset;
  //   const currentIndex = queue.findIndex(
  //   (candidate) => candidate.id === nowPlaying?.id,
  // );
  // const nextIndex = currentIndex + 1;
  // if (duration === 0) {
  //       setNowPlaying(queue[nextIndex]);
  // }
  // }, [nowPlaying, setNowPlaying])

  const clipDateTime = useMemo(() => {
    if (nowPlaying?.array) {
      const timestamp = new Date(nowPlaying?.array[0].timestamp);
      return formatTimestamp(timestamp);
    } else {
      return "";
    }
  }, [nowPlaying]);

  function calcMarkValue(
    playlistStartTime: string | Date,
    startOffset: number,
    detectionTimestamp: string | Date,
  ): number {
    const startTime =
      typeof playlistStartTime === "string"
        ? new Date(playlistStartTime)
        : playlistStartTime;
    const targetTime =
      typeof detectionTimestamp === "string"
        ? new Date(detectionTimestamp)
        : detectionTimestamp;
    const startTimeSeconds = startTime.getTime() / 1000;
    const targetTimeSeconds = targetTime.getTime() / 1000;
    const seconds = targetTimeSeconds - startTimeSeconds - startOffset;
    return +seconds.toFixed(1);
  }

  const duration = useMemo(() => {
    return nowPlaying?.duration;
  }, [nowPlaying]);

  const marks = useMemo(() => {
    return detections?.map((d) => ({
      label: (
        <Tooltip
          title={`
            ${d.newCategory} 
            ${
              d.description !== null && d.description !== undefined
                ? d.description
                : d.comments
                  ? d.comments
                  : ""
            }`}
          arrow
          placement="bottom"
          slotProps={{
            popper: {
              modifiers: [
                {
                  name: "offset",
                  options: {
                    offset: [0, -10],
                  },
                },
              ],
            },
          }}
        >
          <span
            style={{
              width: "20px",
              display: "block",
              height: "10px",
              paddingTop: "10px",
            }}
          >
            {""}
          </span>
        </Tooltip>
      ),
      value: playlistStartTime
        ? calcMarkValue(playlistStartTime.toString(), startOffset, d.timestamp)
        : 0,
      // value: detections?.map(d => playlistStartTime
      //   ? calcMarkValue(playlistStartTime.toString(), startOffset, d.timestamp)
      //   : 0),
    }));
  }, [playlistStartTime, startOffset, detections]);

  return (
    <Stack
      direction="column"
      sx={{
        position: "fixed",
        bottom: 0,
        zIndex: (theme) => theme.zIndex.drawer + 1,
        width: "100%",
      }}
    >
      <AppBar
        position="relative"
        color="base"
        sx={{
          top: "auto",
          height: smDown ? "auto" : "87px",
          padding: "8px 0",
          display: "flex",
          justifyContent: "center",
          alignItems: "center",
          backgroundColor: (theme) => theme.palette.base.main,
        }}
      >
        <Toolbar
          sx={{
            width: "100%",
          }}
        >
          <>
            {nowPlaying?.array && feed && (
              <PlaybarPlayer
                feed={feed}
                image={feed.imageUrl || ""}
                playlistTimestamp={playlistTimestamp}
                startOffset={startOffset}
                endOffset={endOffset}
                duration={duration}
                key={`${startOffset}-${endOffset}`}
                clipDateTime={clipDateTime}
                clipNode={hydrophone || ""}
                clipCount={clipCount}
                marks={marks}
                masterPlayerTimeRef={masterPlayerTimeRef}
              />
            )}
          </>
        </Toolbar>
      </AppBar>
      {mobileMenu && mdDown && mobileMenu}
    </Stack>
  );
}
