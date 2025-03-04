import {
  Box,
  Card,
  CardActionArea,
  CardContent,
  Link,
  Typography,
} from "@mui/material";
import { MutableRefObject } from "react";

import { useFeedsQuery } from "@/graphql/generated";
import { type Candidate } from "@/pages/moderator/candidates";

import { CandidateCardAIPlayer } from "./Player/CandidateCardAIPlayer";
import { CandidateCardPlayer } from "./Player/CandidateCardPlayer";
import { VideoJSPlayer } from "./Player/VideoJS";

export default function CandidateCard(props: {
  candidate: Candidate;
  index: number;
  // changeListState?: (value: number, status: string) => void;
  // command?: string;
  players: MutableRefObject<{ [index: number]: VideoJSPlayer }>;
  playNext: boolean;
}) {
  // get hydrophone feed list
  const feedsQueryResult = useFeedsQuery();
  const feedsData = feedsQueryResult.data?.feeds ?? [];

  const candidate = props.candidate;
  const candidateArray = candidate.array;
  const firstCandidate = candidateArray[candidateArray.length - 1];
  const lastCandidate = candidateArray[0];
  const firstTimestamp = firstCandidate.timestamp;
  const lastTimestamp = lastCandidate.timestamp;
  const firstTimestampString = firstCandidate.timestampString;
  const lastTimestampString = lastCandidate.timestampString;
  const feed = feedsData.find((feed) => feed.id === firstCandidate.feedId);

  const startTimestamp = Math.min(
    ...candidateArray.map((d) => +d.playlistTimestamp),
  );

  const offsetPadding = 15;
  const minOffset = Math.min(...candidateArray.map((d) => +d.playerOffset));

  // const maxOffset = Math.max(...candidateArray.map((d) => +d.playerOffset));
  // instead, ensure that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
  const firstPlaylist = candidateArray.filter(
    (d) => +d.playlistTimestamp === startTimestamp,
  );

  const maxOffset = Math.max(...firstPlaylist.map((d) => +d.playerOffset));
  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;

  return (
    <Card key={firstTimestampString} sx={{ display: "flex" }}>
      <Box
        sx={{
          display: "flex",
          alignItems: "center",
          pl: 1,
          pb: 1,
          minWidth: 250,
        }}
      >
        {feed && candidate.array[0].playlistTimestamp ? (
          <CandidateCardPlayer
            candidate={candidate}
            feed={feed}
            timestamp={startTimestamp}
            startOffset={startOffset}
            endOffset={endOffset}
            index={props.index}
            onPlayerInit={(player) => {
              props.players.current[props.index] = player;
            }}
            onPlay={() => {
              Object.entries(props.players.current).forEach(([key, player]) => {
                if (+key !== props.index) {
                  player.pause();
                }
              });
            }}
            onPlayerEnd={() => {
              if (props.playNext)
                props.players.current[props.index + 1]?.play();
            }}
          />
        ) : candidate.array[0].audioUri ? (
          <>
            <CandidateCardAIPlayer
              audioUri={candidate.array[0].audioUri}
              index={props.index}
              onPlayerInit={(player) => {
                props.players.current[props.index] = player;
              }}
              onPlay={() => {
                Object.entries(props.players.current).forEach(
                  ([key, player]) => {
                    if (+key !== props.index) {
                      player.pause();
                    }
                  },
                );
              }}
              onPlayerEnd={() => {
                if (props.playNext)
                  props.players.current[props.index + 1]?.play();
              }}
            />
          </>
        ) : (
          "no player found"
        )}
      </Box>
      <Link
        href={
          firstTimestamp === lastTimestamp
            ? `${firstTimestamp}`
            : `${firstTimestamp}_${lastTimestamp}`
        }
        sx={{ width: "100%", color: "inherit", textDecoration: "inherit" }}
      >
        <CardActionArea>
          <CardContent>
            <Typography variant="h6" component="div">
              {new Date(lastTimestamp).toLocaleString()}
            </Typography>
            <Typography variant="body1">
              {candidate.hydrophone}
              {" • "}
              {candidate.array.length === 1
                ? candidate.array[0].type === "human"
                  ? "30 seconds"
                  : "1 minute"
                : Math.round(
                      (Date.parse(lastTimestampString) -
                        Date.parse(firstTimestampString)) /
                        (1000 * 60),
                    ) >= 1
                  ? Math.round(
                      (Date.parse(lastTimestampString) -
                        Date.parse(firstTimestampString)) /
                        (1000 * 60),
                    ) + " minutes"
                  : Math.round(
                      (Date.parse(lastTimestampString) -
                        Date.parse(firstTimestampString)) /
                        (1000 * 60 * 60),
                    ) + " seconds"}
              <br />
              {["whale", "vessel", "other", "whale (AI)"]
                .map((item) =>
                  candidate[item as keyof Candidate]
                    ? candidate[item as keyof Candidate] + "  " + item
                    : null,
                )
                .filter((candidate) => candidate !== null)
                .join(" • ")}
              <br />
              {candidate.descriptions ? (
                <span>{"Descriptions: " + candidate.descriptions}</span>
              ) : (
                <br />
              )}
            </Typography>
          </CardContent>
        </CardActionArea>
      </Link>
    </Card>
  );
}
