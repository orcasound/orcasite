import { PauseCircle, PlayCircle } from "@mui/icons-material";
import {
  Box,
  Card,
  CardActionArea,
  CardContent,
  Chip,
  Stack,
  Theme,
  Typography,
  useMediaQuery,
} from "@mui/material";
import { useRouter } from "next/router";

import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { Candidate, CombinedData } from "@/types/DataTypes";
import { formatTimestamp } from "@/utils/time";

import { useComputedPlaybackFields } from "../../hooks/useComputedPlaybackFields";
import formatDuration from "../../utils/masterDataHelpers";

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

export default function CandidateCard(props: { candidate: Candidate }) {
  const {
    nowPlayingCandidate,
    setNowPlayingCandidate,
    setNowPlayingFeed,
    masterPlayerRef,
    masterPlayerStatus,
  } = useNowPlaying();
  const candidate = props.candidate;
  const active = candidate.id === nowPlayingCandidate?.id;
  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  const { feeds } = useData();
  const feed = feeds.find(
    (feed) => feed.id === props.candidate.array[0].feedId,
  );

  const { startOffset, endOffset } = useComputedPlaybackFields(props.candidate);

  const duration = endOffset - startOffset;
  const durationString = formatDuration(startOffset, endOffset);

  function extractHttpLinks(detectionArray: CombinedData[]): string[] {
    const urlRegex = /https?:\/\/\S+/g;
    const sightingsArray = detectionArray.filter((d) => d.type === "sightings");
    return sightingsArray.flatMap((detection) => {
      if (!detection.comments) return [];
      const matches = detection.comments.match(urlRegex);
      return matches || [];
    });
  }

  const imageLinks = extractHttpLinks(props.candidate.array);

  const image = feed ? feed.imageUrl : "";
  const { descriptions } = props.candidate;
  const tagArray = descriptions.match(new RegExp(tagRegex.join("|"), "gi"));
  const uniqueTags = [...new Set(tagArray)];
  const tagObject: { [key: string]: number | undefined } = {};

  uniqueTags.forEach((tag) => {
    const count = tagArray?.filter((el) => el === tag).length;
    tagObject[tag] = count;
  });

  const candidateArray = candidate.array;
  const firstCandidate = candidateArray[0]; // firstCandidate is the earliest time, reports are sorted descending
  const lastCandidate = candidateArray[candidateArray.length - 1]; // lastCandidate is the most recent time
  const firstTimestamp = firstCandidate.timestampString;
  const lastTimestamp = lastCandidate.timestampString;
  const firstTimestampString = firstCandidate.timestampString;
  const lastTimestampString = lastCandidate.timestampString;
  const candidateTitle = formatTimestamp(firstCandidate.timestampString);

  // use these to set href on cards
  const router = useRouter();
  const basePath = router.pathname.replace(/\[.*?\]/g, "").replace(/\/$/, ""); // remove the query in [], then remove any trailing slash
  const candidateHref =
    firstTimestamp === lastTimestamp
      ? `${basePath}/candidate/${firstTimestampString}`
      : `${basePath}/candidate/${firstTimestampString}_${lastTimestampString}`;

  const handlePlay = (candidate: Candidate) => {
    setNowPlayingCandidate(candidate);
    setNowPlayingFeed(null);
    masterPlayerRef?.current?.play();
  };

  const handlePause = () => {
    masterPlayerRef?.current?.pause();
  };

  const iconSize = "40px";

  const playIcon = (
    <PlayCircle
      onClick={() => handlePlay(candidate)}
      sx={{
        height: iconSize,
        width: iconSize,
        cursor: "pointer",
        // marginRight: smDown ? "-8px" : "-4px",
      }}
    />
  );

  const playIconDisabled = (
    <PlayCircle
      sx={{
        opacity: 0.33,
        height: iconSize,
        width: iconSize,
        // marginRight: smDown ? "-8px" : "-4px",
      }}
    />
  );

  const pauseIcon = (
    <PauseCircle
      onClick={() => handlePause()}
      sx={{
        height: iconSize,
        width: iconSize,
        cursor: "pointer",
        // marginRight: smDown ? "-8px" : "-4px",
      }}
    />
  );

  return (
    <Card
      key={firstTimestampString}
      sx={{
        display: "flex",
        flexFlow: "row-reverse",
        width: "100%",
        maxWidth: "100%",
        overflow: "hidden",
        backgroundColor: active ? "rgba(255,255,255,.1)" : "default",
        // border: active ? "1px solid rgba(255,255,255,.25)" : "none",
      }}
    >
      <CardActionArea>
        <CardContent
          sx={{
            display: "flex",
            flexFlow: "column",
            gap: "12px",
            fontSize: smDown ? "14px" : "1rem",
            padding: smDown ? "12px" : "1rem",
          }}
        >
          <Box
            sx={{
              display: "flex",
              justifyContent: "space-between",
              width: "100%",
            }}
          >
            <Link
              // custom Link component based on NextLink, not MUI Link, is required here to persist layout and avoid page reset
              href={candidateHref}
              style={{
                width: "100%",
                color: "inherit",
                textDecoration: "inherit",
              }}
            >
              <Box
                sx={{
                  display: "flex",
                  gap: "1rem",
                  alignItems: "center",
                }}
              >
                <Box
                  sx={{
                    backgroundImage: `url(${image})`,
                    backgroundPosition: "center",
                    backgroundSize: "cover",
                    backgroundRepeat: "no-repeat",
                    width: smDown ? "40px" : "60px",
                    height: smDown ? "40px" : "60px",
                    borderRadius: "4px",
                  }}
                ></Box>
                <Stack>
                  <Typography
                    variant="body1"
                    component="div"
                    sx={{
                      fontWeight: "bold",
                      fontSize: "inherit",
                      // color: active ? (theme) => "antiquewhite" : "default",
                    }}
                  >
                    {candidateTitle}
                  </Typography>
                  <Typography variant="body1" sx={{ fontSize: "inherit" }}>
                    {candidate.hydrophone}
                    {" • "}
                    {durationString}
                  </Typography>
                </Stack>
              </Box>
            </Link>
            <Box>
              {duration > 0
                ? !active
                  ? playIcon
                  : masterPlayerStatus !== "playing"
                    ? playIcon
                    : pauseIcon
                : playIconDisabled}
            </Box>
          </Box>
          <Link
            // custom Link component based on NextLink, not MUI Link, is required here to persist layout and avoid page reset
            href={candidateHref}
            style={{
              width: "100%",
              color: "inherit",
              textDecoration: "inherit",
            }}
          >
            <Typography variant="body1" sx={{ fontSize: "inherit" }}>
              {candidate.clipCount}
              <span style={{ whiteSpace: "pre" }}>{"  "}</span>
              {candidate.descriptions ? (
                <span style={{ color: "rgba(255,255,255,.75)" }}>
                  {candidate.descriptions}
                </span>
              ) : (
                <br />
              )}
            </Typography>

            {tagArray && (
              <Box
                sx={{
                  display: "flex",
                  gap: "10px",
                  padding: "1rem 0 0",
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
            {imageLinks.length > 0 && (
              <Box
                sx={{
                  display: "flex",
                  gap: "10px",
                  padding: "1rem 0",
                  flexWrap: "wrap",
                }}
              >
                {imageLinks.map((link) => (
                  <Box
                    key={link}
                    sx={{
                      height: "100px",
                      width: "100px",
                      backgroundImage: `url(${link})`,
                      backgroundSize: "cover",
                    }}
                  ></Box>
                ))}
              </Box>
            )}
          </Link>
        </CardContent>
      </CardActionArea>
    </Card>
  );
}
