import { Pause, PlayArrow, PlayCircle } from "@mui/icons-material";
import {
  Box,
  Card,
  CardActionArea,
  CardContent,
  Stack,
  Theme,
  Typography,
  useMediaQuery,
} from "@mui/material";

import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { Feed } from "@/graphql/generated";
import useFeedPresence from "@/hooks/useFeedPresence";

type Props = {
  feed: Feed;
};

export default function HydrophoneCard({ feed }: Props) {
  const {
    nowPlayingFeed,
    setNowPlayingFeed,
    setNowPlayingCandidate,
    masterPlayerRef,
    masterPlayerStatus,
  } = useNowPlaying();

  const { autoPlayOnReady } = useData();

  const active = feed.id === nowPlayingFeed?.id;
  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  // use these to set href on cards
  // const router = useRouter();
  // const basePath = router.pathname.replace(/\[.*?\]/g, "").replace(/\/$/, ""); // remove the query in [], then remove any trailing slash
  const feedHref = `/beta/${feed.slug}`;

  const handlePlay = (feed: Feed) => {
    autoPlayOnReady.current = true;
    setNowPlayingFeed(feed);
    setNowPlayingCandidate(null);
    // masterPlayerRef?.current?.play();
    // router.push(`/beta/${feed.slug}`);
  };

  const handlePause = () => {
    masterPlayerRef?.current?.pause();
  };

  const iconSize = "32px";

  const feedPresence = useFeedPresence(feed?.slug);
  const listenerCount = feedPresence?.metas.length ?? 0;

  const playIcon = (
    <PlayArrow
      onClick={() => handlePlay(feed)}
      sx={{
        height: iconSize,
        width: iconSize,
        cursor: "pointer",
      }}
    />
  );

  const _playIconDisabled = (
    <PlayCircle
      sx={{
        opacity: 0.33,
        height: iconSize,
        width: iconSize,
      }}
    />
  );

  const pauseIcon = (
    <Pause
      onClick={() => handlePause()}
      sx={{
        height: iconSize,
        width: iconSize,
        cursor: "pointer",
      }}
    />
  );

  return (
    <Card
      key={feed.id}
      sx={{
        display: "flex",
        flexFlow: "row-reverse",
        width: "100%",
        maxWidth: "100%",
        overflow: "hidden",
        backgroundColor: active
          ? (theme) => theme.palette.base.main
          : "default",
        border: active
          ? "1px solid rgba(255,255,255,.25)"
          : "1px solid transparent",
      }}
    >
      <CardActionArea>
        <CardContent
          sx={{
            display: "flex",
            flexFlow: "column",
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
            <Box
              sx={{
                display: "flex",
                gap: "1.5rem",
                alignItems: "center",
                width: "100%",
              }}
            >
              <Box
                sx={{
                  backgroundImage: `url(${feed.imageUrl})`,
                  backgroundPosition: "center",
                  backgroundSize: "cover",
                  backgroundRepeat: "no-repeat",
                  width: "80px",
                  height: "80px",
                  borderRadius: "8px",
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "center",
                }}
              >
                <div
                  className="hydrophone-play-pause"
                  style={{
                    width: 40,
                    height: 40,
                    borderRadius: 100,
                    backgroundColor: "rgba(0,0,0,.66)",
                    display: "flex",
                    alignItems: "center",
                    justifyContent: "center",
                  }}
                >
                  {!active || masterPlayerStatus !== "playing"
                    ? playIcon
                    : pauseIcon}
                </div>
              </Box>
              <Link
                // custom Link component based on NextLink, not MUI Link, is required here to persist layout and avoid page reset
                href={feedHref}
                onClick={() => (autoPlayOnReady.current = false)}
                className="feed-href"
                sx={{
                  color: "inherit",
                  textDecoration: "inherit",
                  display: "flex",
                  alignItems: "center",
                  flex: 1,
                  height: "100%",
                  "&:hover": {
                    color: "inherit",
                  },
                }}
              >
                <Stack sx={{ flex: 1 }}>
                  <Typography
                    variant="body1"
                    component="div"
                    sx={{
                      fontWeight: "bold",
                      fontSize: "inherit",
                    }}
                  >
                    {feed.name}
                  </Typography>
                  <Typography variant="body1" sx={{ fontSize: "inherit" }}>
                    {`${feed.online && "Live: "}${listenerCount} listener${listenerCount !== 1 ? "s" : ""}`}
                  </Typography>
                </Stack>
              </Link>
              {/* <Link
                href={feedHref}
                style={{
                  color: "inherit",
                  textDecoration: "inherit",
                  display: "flex",
                  alignItems: "center",
                  background: "rgba(0,0,0,.5)",
                  borderRadius: "100%",
                  padding: "4px",
                }}
              >
                <NavigateNext />
              </Link> */}
            </Box>
          </Box>
        </CardContent>
      </CardActionArea>
    </Card>
  );
}
