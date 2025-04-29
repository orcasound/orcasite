import { AppBar, Stack, Toolbar, Typography } from "@mui/material";
import { useEffect, useState } from "react";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";

import { CandidateCardAIPlayer } from "./Player/CandidateCardAIPlayer";
import { CandidateCardPlayer } from "./Player/CandidateCardPlayer";

export default function PlayBar() {
  const { nowPlaying } = useNowPlaying();
  const { feeds } = useData();

  const [playerProps, setPlayerProps] = useState({
    feed: feeds.length > 0 ? feeds[0] : null,
    playlist: 0,
    startOffset: 0,
    endOffset: 0,
    audioUri: "",
  });

  useEffect(() => {
    const candidateArray = nowPlaying.array;
    if (candidateArray && candidateArray.length > 0) {
      const firstDetection = candidateArray[candidateArray.length - 1];
      const lastDetection = candidateArray[0];
      const feed = feeds.find((feed) => feed.id === firstDetection.feedId);

      const playlist =
        candidateArray.length > 0
          ? Math.min(...candidateArray.map((d) => +d.playlistTimestamp))
          : 0;

      const offsetPadding = 15;
      const minOffset =
        candidateArray.length > 0
          ? Math.min(...candidateArray.map((d) => +d.playerOffset))
          : 0;

      // ensure that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
      const firstPlaylist = candidateArray.filter(
        (d) => +d.playlistTimestamp === playlist,
      );

      const maxOffset =
        firstPlaylist.length > 0
          ? Math.max(...firstPlaylist.map((d) => +d.playerOffset))
          : 0;
      const startOffset = Math.max(0, minOffset - offsetPadding);
      const endOffset = maxOffset + offsetPadding;

      console.log("Updating PlayerProps:", {
        startOffset,
        endOffset,
        playlist,
      });

      if (feed) {
        setPlayerProps({
          feed: feed ? feed : feeds[0],
          playlist: playlist,
          startOffset: startOffset,
          endOffset: endOffset,
          audioUri: "",
        });
      }

      if (lastDetection.audioUri) {
        setPlayerProps((p) => ({
          ...p,
          feed: null,
          playlist: 0,
          startOffset: 0,
          endOffset: 0,
          audioUri: lastDetection.audioUri,
        }));
      }
    }
  }, [nowPlaying, feeds]);

  return (
    <AppBar
      position="fixed"
      color="secondary"
      sx={{
        // Keep header above the side drawer
        zIndex: (theme) => theme.zIndex.drawer + 1,
        bottom: 0,
        top: "auto",
        height: "100px",
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
      }}
    >
      <Toolbar>
        {nowPlaying.array && playerProps.feed ? (
          <>
            <Stack>
              <Typography component="h2">{`${new Date(nowPlaying?.array[0].timestamp).toLocaleString()}`}</Typography>
              <Typography>{`${nowPlaying?.array[0].hydrophone}`}</Typography>
            </Stack>
            <CandidateCardPlayer
              feed={playerProps.feed}
              playlistTimestamp={playerProps.playlist}
              startOffset={playerProps.startOffset}
              endOffset={playerProps.endOffset}
              key={playerProps.startOffset + "-" + playerProps.endOffset}
            />
          </>
        ) : nowPlaying.array && playerProps.audioUri.length > 0 ? (
          <>
            <Stack>
              <Typography component="h2">{`${new Date(nowPlaying?.array[0].timestamp).toLocaleString()}`}</Typography>
              <Typography>{`${nowPlaying?.array[0].hydrophone}`}</Typography>
            </Stack>
            <CandidateCardAIPlayer
              audioUri={playerProps.audioUri}
              key={playerProps.audioUri}
            />
          </>
        ) : !nowPlaying.array ||
          (nowPlaying.array &&
            !playerProps.feed &&
            !playerProps.audioUri.length) ? (
          "Press play on any candidate to activate Play bar"
        ) : (
          "Something is wrong"
        )}
      </Toolbar>
    </AppBar>
  );
}
