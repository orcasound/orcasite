import type { Theme } from "@mui/material";
import {
  Box,
  Card,
  CardActionArea,
  CardContent,
  Chip,
  Link,
  Typography,
} from "@mui/material";
import { useMediaQuery } from "@mui/material";
import { MutableRefObject, useCallback } from "react";

import { Feed } from "@/graphql/generated";
import { type Candidate } from "@/pages/moderator/candidates";

import { CandidateCardAIPlayer } from "./Player/CandidateCardAIPlayer";
import { CandidateCardPlayer } from "./Player/CandidateCardPlayer";
import { VideoJSPlayer } from "./Player/VideoJS";

const tagRegex = [
  "s[0-9]+",
  "srkw",
  "call",
  "whistle",
  "click",
  "j pod",
  "j-pod",
  "k pod",
  "k-pod",
  "l pod",
  "l-pod",
  "biggs",
  "bigg's",
];

export default function CandidateCard(props: {
  candidate: Candidate;
  index: number;
  // changeListState?: (value: number, status: string) => void;
  // command?: string;
  players: MutableRefObject<{ [index: number]: VideoJSPlayer }>;
  playNext: boolean;
  feeds: Feed[];
}) {
  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  const { descriptions } = props.candidate;
  const tagArray = descriptions.match(new RegExp(tagRegex.join("|"), "gi"));
  const uniqueTags = [...new Set(tagArray)];
  const tagObject: { [key: string]: number | undefined } = {};

  uniqueTags.forEach((tag) => {
    const count = tagArray?.filter((el) => el === tag).length;
    tagObject[tag] = count;
  });

  const candidate = props.candidate;
  const candidateArray = candidate.array;
  const firstCandidate = candidateArray[candidateArray.length - 1];
  const lastCandidate = candidateArray[0];
  const firstTimestamp = firstCandidate.timestamp;
  const lastTimestamp = lastCandidate.timestamp;
  const firstTimestampString = firstCandidate.timestampString;
  const lastTimestampString = lastCandidate.timestampString;
  const feed =
    props.feeds.find((feed) => feed.id === firstCandidate.feedId) ||
    props.feeds.find((feed) => feed.id === lastCandidate.feedId);

  const humanReports = candidateArray.filter(
    (d) => d.playlistTimestamp !== undefined && d.playerOffset !== undefined,
  );

  const startPlaylistTimestamp = Math.min(
    ...humanReports.map((d) => +d.playlistTimestamp),
  );

  const offsetPadding = 15;
  const minOffset = Math.min(...humanReports.map((d) => +d.playerOffset));

  // const maxOffset = Math.max(...candidateArray.map((d) => +d.playerOffset));
  // instead, ensure that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
  const firstPlaylist = humanReports.filter(
    (d) => +d.playlistTimestamp === startPlaylistTimestamp,
  );

  const maxOffset = Math.max(...firstPlaylist.map((d) => +d.playerOffset));
  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;

  const onPlay = useCallback(() => {
    Object.entries(props.players.current).forEach(([key, player]) => {
      if (+key !== props.index) {
        player.pause();
      }
    });
  }, [props.index, props.players]);

  const onPlayerEnd = useCallback(() => {
    if (props.playNext) props.players.current[props.index + 1]?.play();
  }, [props.playNext, props.index, props.players]);

  const onPlayerInit = useCallback(
    (player: VideoJSPlayer) => {
      props.players.current[props.index] = player;
    },
    [props.index, props.players],
  );

  return (
    <Card
      key={firstTimestampString}
      sx={{
        display: "flex",
        flexFlow: lgUp ? "row" : "column-reverse",
        width: "100%",
        maxWidth: "100%",
        overflow: "hidden",
      }}
    >
      <Box
        sx={{
          display: "flex",
          alignItems: "center",
          pl: 1,
          pb: 1,
          minWidth: lgUp ? 250 : 0,
        }}
      >
        {feed && candidate.array[0].playlistTimestamp ? (
          <CandidateCardPlayer
            feed={feed}
            timestamp={startPlaylistTimestamp}
            startOffset={startOffset}
            endOffset={endOffset}
            // index={props.index}
            onPlayerInit={onPlayerInit}
            onPlay={onPlay}
            onPlayerEnd={onPlayerEnd}
          />
        ) : candidate.array[0].audioUri ? (
          <>
            <CandidateCardAIPlayer
              audioUri={candidate.array[0].audioUri}
              // index={props.index}
              onPlayerInit={onPlayerInit}
              onPlay={onPlay}
              onPlayerEnd={onPlayerEnd}
            />
          </>
        ) : (
          "startOffset: " +
          startOffset +
          " endOffset: " +
          endOffset +
          " timestamp: " +
          startPlaylistTimestamp +
          " feed: " +
          feed
        )}
      </Box>
      <Link
        // href needs a slash before so it isn't relative to folder path
        href={
          firstTimestamp === lastTimestamp
            ? `/moderator/${firstTimestampString}`
            : `/moderator/${firstTimestampString}_${lastTimestampString}`
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
            </Typography>
            {tagArray && (
              <Box
                sx={{
                  display: "flex",
                  gap: "10px",
                  padding: "1rem 0",
                  flexWrap: "wrap",
                }}
              >
                {Object.entries(tagObject).map(([tag]) => (
                  <Chip
                    label={`${tag}`}
                    key={tag}
                    variant="filled"
                    sx={{
                      fontSize: "14px",
                    }}
                  />
                ))}
              </Box>
            )}
            <Typography>
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
