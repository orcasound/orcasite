import { AppBar, Stack, Theme, Toolbar, useMediaQuery } from "@mui/material";
import React, { useMemo } from "react";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";

import { useComputedPlaybackFields } from "./CandidateList/useComputedPlaybackFields";
import { PlaybarPlayer } from "./Player/PlaybarPlayer";

export default function PlayBar({
  mobileMenu,
}: {
  mobileMenu?: React.ReactNode;
}) {
  const { nowPlaying } = useNowPlaying();
  const { feeds } = useData();
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  const detections = nowPlaying?.array;
  const hydrophone = nowPlaying?.hydrophone;
  const feed = feeds.find((feed) => feed.id === detections?.[0]?.feedId);

  const { playlistTimestamp, startOffset, endOffset } =
    useComputedPlaybackFields(nowPlaying?.array, feed?.id);

  const clipDateTime = useMemo(() => {
    if (nowPlaying?.array) {
      return new Date(nowPlaying?.array[0].timestamp).toLocaleString();
    } else {
      return "";
    }
  }, [nowPlaying]);

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
          height: smDown ? "56px" : "87px",
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
                key={`${startOffset}-${endOffset}`}
                clipDateTime={clipDateTime}
                clipNode={hydrophone || ""}
              />
            )}
          </>
        </Toolbar>
      </AppBar>
      {mobileMenu && mdDown && mobileMenu}
    </Stack>
  );
}
