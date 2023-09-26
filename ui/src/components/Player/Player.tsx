import { Box, Typography, useMediaQuery } from "@mui/material";
import { styled, useTheme } from "@mui/material/styles";
import dynamic from "next/dynamic";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import Marquee from "react-fast-marquee";

import type { Feed } from "@/graphql/generated";
import useFeedPresence from "@/hooks/useFeedPresence";
import { useTimestampFetcher } from "@/hooks/useTimestampFetcher";
import {
  displayDesktopOnly,
  displayMobileOnly,
  mobileOnly,
} from "@/styles/responsive";
import { analytics } from "@/utils/analytics";
import { useIsRelativeOverflow } from "@/utils/layout";

import { TitlePopover } from "../TitlePopover";
import DetectionButton from "./DetectionButton";
import DetectionDialog from "./DetectionDialog";
import ListenerCount from "./ListenerCount";
import PlayPauseButton from "./PlayPauseButton";
import { type VideoJSPlayer } from "./VideoJS";

// dynamically import VideoJS to speed up initial page load
const VideoJS = dynamic(() => import("./VideoJS"));

export type PlayerStatus = "idle" | "loading" | "playing" | "paused" | "error";

export default function Player({
  currentFeed,
}: {
  currentFeed?: Pick<
    Feed,
    "id" | "slug" | "nodeName" | "name" | "latLng" | "imageUrl"
  >;
}) {
  const [playerStatus, setPlayerStatus] = useState<PlayerStatus>("idle");
  const playerRef = useRef<VideoJSPlayer | null>(null);

  const { timestamp, hlsURI } = useTimestampFetcher(currentFeed?.nodeName);

  const feedPresence = useFeedPresence(currentFeed?.slug);
  const listenerCount = feedPresence?.metas.length ?? 0;

  const playerText = currentFeed
    ? `${currentFeed.name} - ${currentFeed.nodeName}`
    : "Select a location to start listening live";

  const playerTextContainerRef = useRef<HTMLElement>();
  const playerTextRef = useRef<HTMLElement>();
  const playerTextOverflowing = useIsRelativeOverflow(
    playerTextContainerRef,
    playerTextRef,
  );
  const theme = useTheme();
  const isDesktop = useMediaQuery(theme.breakpoints.up("sm"));
  const [playMarquee, setPlayMarquee] = useState(false);

  useEffect(() => {
    setTimeout(() => {
      setPlayMarquee(true);
    }, 3000);
  }, [])

  const playerOptions = useMemo(
    () => ({
      poster: currentFeed?.imageUrl,
      autoplay: true,
      flash: {
        hls: {
          overrideNative: true,
        },
      },
      html5: {
        hls: {
          overrideNative: true,
        },
      },
      sources: currentFeed?.nodeName
        ? [
            {
              // If hlsURI isn't set, use a dummy URI to trigger an error
              // The dummy URI doesn't actually exist, it should return 404
              // This is the only way to get videojs to throw an error, otherwise
              // it just won't initialize (if src is undefined/null/empty))
              src: hlsURI ?? `${currentFeed?.nodeName}/404`,
              type: "application/x-mpegurl",
            },
          ]
        : [],
    }),
    [hlsURI, currentFeed?.nodeName, currentFeed?.imageUrl],
  );

  const handleReady = useCallback((player: VideoJSPlayer) => {
    playerRef.current = player;

    player.on("playing", () => setPlayerStatus("playing"));
    player.on("pause", () => setPlayerStatus("paused"));
    player.on("waiting", () => setPlayerStatus("loading"));
    player.on("error", () => setPlayerStatus("error"));
  }, []);

  const handlePlayPauseClick = async () => {
    const player = playerRef.current;

    if (playerStatus === "error") {
      setPlayerStatus("idle");
      return;
    }

    if (!player) {
      setPlayerStatus("error");
      return;
    }

    try {
      if (playerStatus === "loading" || playerStatus === "playing") {
        await player.pause();
        currentFeed?.slug && analytics.stream.paused(currentFeed.slug);
      } else {
        await player.play();
        currentFeed?.slug && analytics.stream.started(currentFeed.slug);
      }
    } catch (e) {
      console.error(e);
      // AbortError is thrown if pause() is called while play() is still loading (e.g. if segments are 404ing)
      // It's not important, so don't show this error to the user
      if (e instanceof DOMException && e.name === "AbortError") return;
      setPlayerStatus("error");
    }
  };

  useEffect(() => {
    if (process.env.NODE_ENV === "development" && hlsURI) {
      console.log(`New stream instance: ${hlsURI}`);
    }

    return () => {
      setPlayerStatus("idle");
    };
  }, [hlsURI, currentFeed?.nodeName]);

  return (
    <Box
      sx={(theme) => ({
        minHeight: theme.spacing(10),
        color: "base.contrastText",
        backgroundColor: "base.main",
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between",
        px: [0, 2],
        position: "relative",
        [mobileOnly(theme)]: {
          position: "fixed",
          bottom: 0,
          left: 0,
          right: 0,
        },
        // Keep player above the sliding drawer
        zIndex: theme.zIndex.drawer + 1,
      })}
    >
      <Box display="none">
        <VideoJS options={playerOptions} onReady={handleReady} />
      </Box>
      {(playerStatus === "playing" || playerStatus === "loading") &&
        currentFeed && (
          <DetectionDialog
            isPlaying={playerStatus === "playing"}
            feed={currentFeed}
            timestamp={timestamp}
            getPlayerTime={() => playerRef.current?.currentTime()}
            listenerCount={listenerCount}
          >
            <DetectionButton />
          </DetectionDialog>
        )}
      <Box mx={1}>
        <PlayPauseButton
          playerStatus={playerStatus}
          onClick={handlePlayPauseClick}
          disabled={!currentFeed}
        />
      </Box>
      <Box mx={1}>{currentFeed && <ListenerCount count={listenerCount} />}</Box>
      <Box
        sx={{
          overflow: "hidden",
        }}
        ref={playerTextContainerRef}
        title={playerText}
      >
        <TitlePopover title={playerText}>
          <Typography
            sx={{
              ...displayDesktopOnly,
              px: 1,
              whiteSpace: "nowrap",
              textOverflow: "ellipsis",
              overflow: "hidden",
              width: 1,
            }}
          >
            {playerText}
          </Typography>
        </TitlePopover>
        <Box
          sx={displayMobileOnly}
          onClick={() => {
            setPlayMarquee(!playMarquee);
            analytics.stream.playerTextClicked(playerText);
          }}
        >
          <Marquee
            speed={15}
            play={playerTextOverflowing && !isDesktop && playMarquee}
            key={`${playerText}-${playerTextOverflowing}-${playMarquee}`}
          >
            <Box ref={playerTextRef}>
              <Typography
                sx={{
                  mx: 1,
                }}
              >
                {playerText}
              </Typography>
            </Box>
          </Marquee>
        </Box>
      </Box>
      <Box
        sx={{
          ...displayDesktopOnly,
          mx: 1,
          flexGrow: 1,
          textAlign: "end",
          whiteSpace: "nowrap",
          textOverflow: "ellipsis",
          overflow: "hidden",
        }}
        title={
          currentFeed?.latLng &&
          `${currentFeed.latLng.lat}, ${currentFeed.latLng.lng}`
        }
      >
        <TitlePopover
          title={
            currentFeed &&
            `${currentFeed.latLng.lat}, ${currentFeed.latLng.lng}`
          }
        >
          {currentFeed &&
            `${currentFeed.latLng.lat}, ${currentFeed.latLng.lng}`}
        </TitlePopover>
      </Box>
    </Box>
  );
}

// Utility component to help with spacing
// Just a box that's the same height as the player
export const PlayerSpacer = styled(Box)(({ theme }) => ({
  height: theme.spacing(10),
}));
