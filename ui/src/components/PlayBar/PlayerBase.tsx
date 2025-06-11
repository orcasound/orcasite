import "videojs-offset";

import {
  AppBar,
  Box,
  Stack,
  Theme,
  Toolbar,
  Typography,
  useMediaQuery,
} from "@mui/material";
import dynamic from "next/dynamic";
import { MutableRefObject, ReactNode, SetStateAction, useMemo } from "react";

import PlayBarPlayPauseButton from "@/components/PlayBar/CandidatePlayPauseButton";
import { type PlayerStatus } from "@/components/Player/Player";
import { VideoJSOptions } from "@/components/Player/VideoJS";
import { type VideoJSPlayer } from "@/components/Player/VideoJS";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
// import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";

import DetectionButton from "../CandidateList/DetectionButtonBeta";
import DetectionDialog from "../CandidateList/DetectionDialogBeta";
import Link from "../Link";
import PlayPauseButton from "../Player/PlayPauseButton";
import { PlaybarSlider } from "./PlaybarSlider";

// dynamically import VideoJS to speed up initial page load
const VideoJS = dynamic(() => import("@/components/Player/VideoJS"));

type PlayerBaseProps = {
  // required for either Candidate or Feed
  type: "candidate" | "feed";
  playerOptions: VideoJSOptions;
  handleReady: (player: VideoJSPlayer) => void;
  playerStatus: PlayerStatus;
  // setPlayerStatus: React.Dispatch<SetStateAction<PlayerStatus>>;
  feed: Feed;
  playerRef: MutableRefObject<VideoJSPlayer | null>;
  handlePlayPauseClickCandidate?: () => void;
  handlePlayPauseClickFeed?: () => Promise<void>;
  image?: string | undefined;
  playerTitle: string | undefined; // change this to player title
  playerSubtitle: string | undefined; // change this to player subtitle
  setPlaybarExpanded: React.Dispatch<SetStateAction<boolean>>;

  // Feed only
  timestamp?: number | undefined;
  listenerCount?: number;
  onAudioPlay?: (() => void) | undefined;

  // Candidate only
  startOffset?: number;
  endOffset?: number;
  duration?: string | undefined;
  marks?: { label: string | ReactNode; value: number }[] | undefined;
  playerTime?: number;
};

export function PlayerBase({
  type,
  playerOptions,
  handleReady,
  playerStatus,
  feed,
  playerRef,
  handlePlayPauseClickCandidate,
  handlePlayPauseClickFeed,
  listenerCount = 0,
  startOffset = 0,
  endOffset = 0,
  image = "",
  playerTitle = "",
  playerSubtitle = "",
  duration = "",
  marks,
  playerTime = 0,
  timestamp,
}: PlayerBaseProps) {
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const { nowPlayingCandidate, nowPlayingFeed } = useNowPlaying();
  const { feeds } = useData();
  const feedSlug = useMemo(() => {
    if (nowPlayingCandidate) {
      const feed =
        feeds.find((f) => f.id === nowPlayingCandidate.feedId) ?? null;
      return feed ? feed.slug : "";
    } else if (nowPlayingFeed) {
      return nowPlayingFeed.slug;
    } else {
      return "";
    }
  }, [nowPlayingCandidate, nowPlayingFeed, feeds]);
  const candidateId = useMemo(() => {
    if (nowPlayingCandidate) {
      return nowPlayingCandidate.id;
    } else {
      return "";
    }
  }, [nowPlayingCandidate]);
  const href =
    candidateId.length > 0
      ? `/beta/${feedSlug}/${candidateId}`
      : `/beta/${feedSlug}/candidates`;

  const slider = (
    <PlaybarSlider
      marks={marks}
      playerRef={playerRef}
      playerTime={playerTime}
      startOffset={startOffset}
      endOffset={endOffset}
    />
  );

  return (
    <>
      <AppBar
        position="relative"
        color="base"
        sx={{
          top: "auto",
          height: "auto",
          padding: "6px 0",
          justifyContent: "center",
          alignItems: "center",
          backgroundColor: "base.main",
          borderRadius: "8px",
          border: "1px solid rgba(255,255,255,.25)",
          display: "flex",
        }}
      >
        <Toolbar
          className="toolbar"
          sx={{
            width: "100%",
            px: "1rem !important",
          }}
        >
          <Stack spacing={1} sx={{ width: "100%" }}>
            <Box
              sx={(theme) => ({
                minHeight: smDown ? 0 : theme.spacing(10),
                display: "flex",
                alignItems: "center",
                justifyContent: "flex-start",
                px: [0, 2],
                position: "relative",
                // className: "candidate-card-player",
                // Keep player above the sliding drawer
                zIndex: theme.zIndex.drawer + 1,
                width: "100%",
                // flexFlow: smDown ? "row-reverse" : "row",
                gap: smDown ? 2 : 3,
                marginRight: smDown ? 0 : "2rem",
              })}
            >
              <Box display="none" id="video-js">
                <VideoJS
                  options={playerOptions}
                  onReady={handleReady}
                  key={`${startOffset}-${endOffset}`}
                />
              </Box>
              <Box ml={0} id="play-pause-button">
                {handlePlayPauseClickCandidate && (
                  <PlayBarPlayPauseButton
                    playerStatus={playerStatus}
                    onClick={handlePlayPauseClickCandidate}
                    disabled={!feed}
                  />
                )}
                {handlePlayPauseClickFeed && (
                  <PlayPauseButton
                    playerStatus={playerStatus}
                    onClick={handlePlayPauseClickFeed}
                    disabled={!feed}
                  />
                )}
              </Box>
              <Link
                href={href}
                sx={{
                  textDecoration: "none",
                  "&:hover": {
                    color: "text.primary",
                  },
                }}
              >
                <Stack
                  direction="row"
                  width="100%"
                  spacing={smDown ? 2 : 3}
                  sx={{ overflow: "hidden" }}
                >
                  <Box
                    sx={{
                      backgroundImage: `url(${image})`,
                      backgroundPosition: "center",
                      backgroundSize: "cover",
                      backgroundRepeat: "no-repeat",
                      minWidth: smDown ? "40px" : "60px",
                      width: smDown ? "40px" : "60px",
                      height: smDown ? "40px" : "60px",
                      borderRadius: "4px",
                    }}
                  ></Box>
                  <Box
                    sx={{
                      display: "flex",
                      flexDirection: "column",
                      width: "100%",
                      justifyContent: "center",
                    }}
                  >
                    <Typography
                      component="h2"
                      sx={{
                        whiteSpace: "nowrap",
                        fontSize: smDown ? "14px" : "1rem",
                      }}
                    >
                      <span style={{ fontWeight: "bold" }}>{playerTitle}</span>
                      {smDown ? <br /> : " · "}
                      {playerSubtitle && playerSubtitle}
                      {type === "feed" &&
                        `${listenerCount} listener${listenerCount !== 1 ? "s" : ""}`}
                      {type === "candidate" && " · " + duration}
                    </Typography>
                    {!smDown && nowPlayingCandidate && slider}
                  </Box>
                </Stack>
              </Link>
            </Box>
            {smDown && nowPlayingCandidate && slider}
          </Stack>
          {(playerStatus === "playing" || playerStatus === "loading") &&
            feed &&
            nowPlayingFeed && (
              <DetectionDialog
                isPlaying={playerStatus === "playing"}
                feed={feed}
                timestamp={timestamp}
                getPlayerTime={() => playerRef.current?.currentTime()}
                listenerCount={listenerCount}
              >
                <DetectionButton />
              </DetectionDialog>
            )}
        </Toolbar>
      </AppBar>
    </>
  );
}
