import { AppBar, Toolbar } from "@mui/material";
import { useEffect, useMemo, useState } from "react";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";

import { PlaybarAIPlayer } from "./Player/PlaybarAIPlayer";
import { PlaybarPlayer } from "./Player/PlaybarPlayer";

export default function PlayBar() {
  const { nowPlaying } = useNowPlaying();
  const { feeds } = useData();

  const [playerProps, setPlayerProps] = useState({
    feed: feeds.length > 0 ? feeds[0] : null,
    image: feeds.length > 0 ? feeds[0].imageUrl : "",
    playlist: 0,
    startOffset: 0,
    endOffset: 0,
    audioUri: "",
  });

  useEffect(() => {
    const candidateArray = nowPlaying?.array;
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

      // there will only be a feed if there are human detections in the candidate
      if (feed) {
        setPlayerProps({
          feed: feed ? feed : feeds[0],
          image: feed ? feed.imageUrl : feeds[0].imageUrl,
          playlist: playlist,
          startOffset: startOffset,
          endOffset: endOffset,
          audioUri: "",
        });
        // otherwise, grab the audio clip from the earliest AI detection
      } else if (lastDetection.audioUri) {
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

  const clipDateTime = useMemo(() => {
    if (nowPlaying?.array) {
      return new Date(nowPlaying?.array[0].timestamp).toLocaleString();
    } else {
      return "";
    }
  }, [nowPlaying]);

  const clipNode = useMemo(() => {
    if (nowPlaying) {
      return nowPlaying.hydrophone;
    } else {
      return "";
    }
  }, [nowPlaying]);

  return (
    <AppBar
      position="fixed"
      color="base"
      sx={{
        // Keep header above the side drawer
        zIndex: (theme) => theme.zIndex.drawer + 1,
        bottom: 0,
        top: "auto",
        height: "87px",
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
        {nowPlaying?.array && playerProps.feed ? (
          <>
            <PlaybarPlayer
              feed={playerProps.feed}
              image={playerProps.image?.toString()}
              playlistTimestamp={playerProps.playlist}
              startOffset={playerProps.startOffset}
              endOffset={playerProps.endOffset}
              key={playerProps.startOffset + "-" + playerProps.endOffset}
              clipDateTime={clipDateTime}
              clipNode={clipNode}
            />
          </>
        ) : nowPlaying?.array && playerProps.audioUri.length > 0 ? (
          <>
            <PlaybarAIPlayer
              image={playerProps.image?.toString()}
              audioUri={playerProps.audioUri}
              key={playerProps.audioUri}
              clipDateTime={clipDateTime}
              clipNode={clipNode}
            />
          </>
        ) : !nowPlaying?.array ||
          (nowPlaying?.array &&
            !playerProps.feed &&
            !playerProps.audioUri.length) ? (
          "Loading player..."
        ) : (
          "Something is wrong"
        )}
      </Toolbar>
    </AppBar>
  );
}
