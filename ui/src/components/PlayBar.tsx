import { AppBar, Toolbar, Typography } from "@mui/material";
import { useEffect, useMemo, useState } from "react";

import { useFeedsQuery } from "@/graphql/generated";
import { Candidate } from "@/pages/moderator/candidates";

import { CandidateCardAIPlayer } from "./Player/CandidateCardAIPlayer";
import { CandidateCardPlayer } from "./Player/CandidateCardPlayer";

export default function PlayBar({ candidate }: { candidate: Candidate }) {
  // get hydrophone feed list
  const feedsQueryResult = useFeedsQuery();
  const feedsData = useMemo(() => {
    const feeds = feedsQueryResult.data?.feeds ?? [];
    return feeds;
  }, [feedsQueryResult.data?.feeds]);

  const [playerProps, setPlayerProps] = useState({
    feed: feedsData.length > 0 ? feedsData[0] : null,
    timestamp: 0,
    startOffset: 0,
    endOffset: 0,
    audioUri: "",
  });

  useEffect(() => {
    const candidateArray = candidate.array;
    if (candidateArray) {
      const firstDetection = candidateArray[candidateArray.length - 1];
      const lastDetection = candidateArray[0];
      const feed = feedsData.find((feed) => feed.id === firstDetection.feedId);

      const startTimestamp = Math.min(
        ...candidateArray.map((d) => +d.playlistTimestamp),
      );

      const offsetPadding = 15;
      const minOffset = Math.min(...candidateArray.map((d) => +d.playerOffset));

      // ensure that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
      const firstPlaylist = candidateArray.filter(
        (d) => +d.playlistTimestamp === startTimestamp,
      );

      const maxOffset = Math.max(...firstPlaylist.map((d) => +d.playerOffset));
      const startOffset = Math.max(0, minOffset - offsetPadding);
      const endOffset = maxOffset + offsetPadding;

      feed &&
        setPlayerProps({
          feed: feed ? feed : feedsData[0],
          timestamp: startTimestamp,
          startOffset: startOffset,
          endOffset: endOffset,
          audioUri: "",
        });

      lastDetection.audioUri &&
        setPlayerProps((p) => ({
          ...p,
          timestamp: startTimestamp,
          audioUri: lastDetection.audioUri,
        }));
    }
  }, [candidate, feedsData]);

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
        <pre style={{ color: "white", display: "none" }}>
          {JSON.stringify(candidate)}
        </pre>
        {candidate.array && playerProps.feed ? (
          <>
            <Typography>{`${playerProps.timestamp}`}</Typography>
            <CandidateCardPlayer
              feed={playerProps.feed}
              timestamp={playerProps.timestamp}
              startOffset={playerProps.startOffset}
              endOffset={playerProps.endOffset}
            />
          </>
        ) : candidate.array && playerProps.audioUri.length ? (
          <CandidateCardAIPlayer audioUri={playerProps.audioUri} />
        ) : (
          "Press play on any candidate to activate Play bar"
        )}
      </Toolbar>
    </AppBar>
  );
}
