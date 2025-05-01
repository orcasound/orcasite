import { PauseCircle, PlayCircle } from "@mui/icons-material";
import {
  Box,
  Card,
  CardActionArea,
  CardContent,
  Chip,
  Stack,
  Typography,
} from "@mui/material";

import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { Candidate } from "@/types/DataTypes";

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
  // index: number;
  // players: MutableRefObject<{ [index: number]: VideoJSPlayer }>;
  // feeds: Feed[];
}) {
  const { nowPlaying, setNowPlaying, masterPlayerRef, masterPlayerStatus } =
    useNowPlaying();

  const { feeds } = useData();
  const feed = feeds.find(
    (feed) => feed.id === props.candidate.array[0].feedId,
  );

  const image = feed ? feed.imageUrl : "";
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

  const handlePlay = (candidate: Candidate) => {
    setNowPlaying(candidate);
    masterPlayerRef?.current?.play();
  };

  const handlePause = () => {
    masterPlayerRef?.current?.pause();
  };

  const iconSize = "48px";

  const playIcon = (
    <PlayCircle
      onClick={() => handlePlay(candidate)}
      sx={{
        height: iconSize,
        width: iconSize,
      }}
    />
  );

  const pauseIcon = (
    <PauseCircle
      onClick={() => handlePause()}
      sx={{
        height: iconSize,
        width: iconSize,
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
      }}
    >
      <Box
        sx={{
          display: "flex",
          alignItems: "flex-start",
          p: 2,
        }}
      >
        {candidate.id !== nowPlaying.id
          ? playIcon
          : masterPlayerStatus === "paused"
            ? playIcon
            : pauseIcon}
      </Box>
      <Link
        // custom Link component based on NextLink, not MUI Link, is required here to persist layout and avoid page
        // href needs a slash before so it isn't relative to folder path
        href={
          firstTimestamp === lastTimestamp
            ? `/moderator/${firstTimestampString}`
            : `/moderator/${firstTimestampString}_${lastTimestampString}`
        }
        style={{ width: "100%", color: "inherit", textDecoration: "inherit" }}
      >
        <CardActionArea>
          <CardContent
            sx={{
              display: "flex",
              flexFlow: "column",
              gap: "1em",
            }}
          >
            <Box
              sx={{
                display: "flex",
                gap: "1.5em",
                alignItems: "center",
              }}
            >
              <Box
                sx={{
                  backgroundImage: `url(${image})`,
                  backgroundPosition: "center",
                  backgroundSize: "cover",
                  backgroundRepeat: "no-repeat",
                  width: "60px",
                  height: "60px",
                  borderRadius: "4px",
                }}
              ></Box>
              <Stack>
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
                </Typography>
              </Stack>
            </Box>
            <Box>
              <Typography variant="body1">
                {["whale", "vessel", "other", "whale (AI)"]
                  .map((item) =>
                    candidate[item as keyof Candidate]
                      ? candidate[item as keyof Candidate] + "  " + item
                      : null,
                  )
                  .filter((candidate) => candidate !== null)
                  .join(" • ")}
                <span style={{ whiteSpace: "pre" }}> </span>
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
            </Box>
          </CardContent>
        </CardActionArea>
      </Link>
    </Card>
  );
}
