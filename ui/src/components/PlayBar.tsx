import { AppBar, Stack, Toolbar, Typography } from "@mui/material";
import { useEffect, useMemo, useState } from "react";

import { useNowPlaying } from "@/context/NowPlayingContext";
import { useFeedsQuery } from "@/graphql/generated";

import { CandidateCardAIPlayer } from "./Player/CandidateCardAIPlayer";
import { CandidateCardPlayer } from "./Player/CandidateCardPlayer";

export default function PlayBar() {
  const { nowPlaying } = useNowPlaying();
  // get hydrophone feed list
  const feedsQueryResult = useFeedsQuery();
  const feedsData = useMemo(() => {
    const feeds = feedsQueryResult.data?.feeds ?? [];
    return feeds;
  }, [feedsQueryResult.data?.feeds]);

  const [playerProps, setPlayerProps] = useState({
    feed: feedsData.length > 0 ? feedsData[0] : null,
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
      const feed = feedsData.find((feed) => feed.id === firstDetection.feedId);

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

      feed &&
        setPlayerProps({
          feed: feed ? feed : feedsData[0],
          playlist: playlist,
          startOffset: startOffset,
          endOffset: endOffset,
          audioUri: "",
        });

      lastDetection.audioUri &&
        setPlayerProps((p) => ({
          ...p,
          feed: null,
          playlist: 0,
          startOffset: 0,
          endOffset: 0,
          audioUri: lastDetection.audioUri,
        }));
    }
  }, [nowPlaying, feedsData]);

  return (
    <AppBar
      position="fixed"
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
              playlist={playerProps.playlist}
              startOffset={playerProps.startOffset}
              endOffset={playerProps.endOffset}
            />
          </>
        ) : nowPlaying.array && playerProps.audioUri.length > 0 ? (
          <>
            <Stack>
              <Typography component="h2">{`${new Date(nowPlaying?.array[0].timestamp).toLocaleString()}`}</Typography>
              <Typography>{`${nowPlaying?.array[0].hydrophone}`}</Typography>
            </Stack>
            <CandidateCardAIPlayer audioUri={playerProps.audioUri} />
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
