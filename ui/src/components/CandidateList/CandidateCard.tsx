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
import formatDuration from "@/utils/masterDataHelpers";
import { formatTimestamp } from "@/utils/time";

import { useComputedPlaybackFields } from "../../hooks/beta/useComputedPlaybackFields";

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
  const router = useRouter();

  const candidate = props.candidate;
  const active = candidate.id === nowPlayingCandidate?.id;
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const { feeds, autoPlayOnReady } = useData();
  const feed = feeds.find(
    (feed) => feed.id === props.candidate.array[0].feedId,
  );

  const { duration, durationString } = useComputedPlaybackFields(
    props.candidate,
  );

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

  const candidateTitle = formatTimestamp(candidate.startTimestamp);

  // if the card is rendered on feed detail, show candidateFeedHref
  // const feedDetailHref = `/beta/${feed?.slug}/candidates`;
  const feedDetailCandidateHref = `/beta/${feed?.slug}/${candidate.id}`;

  // if the card is rendered on browse all candidates, show candidateBrowseHref
  // const allCandidatesHref = `/beta/candidates`;
  // 6/5/25 -- abandoning the idea of a candidate detail next to all candidates browse, always goes to feed detail
  // const allCandidatesDetailHref = `/beta/candidates/${feed?.slug}/${candidate.id}`;

  const href = feedDetailCandidateHref;

  const handlePlay = (candidate: Candidate) => {
    autoPlayOnReady.current = true;
    setNowPlayingCandidate(candidate);
    setNowPlayingFeed(null);
    if (!smDown) router.push(href);

    const player = masterPlayerRef?.current;
    if (player && player !== null && typeof player.play === "function") {
      player.play();
    }
  };

  const handlePause = () => {
    const player = masterPlayerRef?.current;
    if (player && typeof player.pause === "function") {
      player.pause();
    }
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
        opacity: 0,
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
      }}
    />
  );

  const currentTimeSeconds = new Date().getTime() / 1000;
  const timestampSeconds = new Date(candidate.startTimestamp).getTime() / 1000;
  const timeAgoString = formatDuration(timestampSeconds, currentTimeSeconds);

  return (
    <Card
      key={candidate.id}
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
              gap: "1rem",
            }}
          >
            <Link
              // custom Link component based on NextLink, not MUI Link, is required here to persist layout and avoid page reset
              href={href}
              onClick={() => {
                autoPlayOnReady.current = false;
                setNowPlayingCandidate(candidate);
                setNowPlayingFeed(null);
              }}
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
                    minWidth: smDown ? "40px" : "60px",
                    minHeight: smDown ? "40px" : "60px",
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
                    {" · "}
                    {timeAgoString} ago
                    {" · "}
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
            href={href}
            onClick={() => (autoPlayOnReady.current = false)}
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
