import { Box } from "@mui/material";
import dynamic from "next/dynamic";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";

import type { Feed } from "@/graphql/generated";
import useFeedPresence from "@/hooks/useFeedPresence";
import useTimestampFetcher from "@/hooks/useTimestampFetcher";

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
  currentFeed?: Pick<Feed, "id" | "slug" | "nodeName" | "name" | "latLng">;
}) {
  const [playerStatus, setPlayerStatus] = useState<PlayerStatus>("idle");
  const playerRef = useRef<VideoJSPlayer | null>(null);

  const { timestamp, hlsURI } = useTimestampFetcher(
    typeof currentFeed?.nodeName === "string"
      ? currentFeed?.nodeName
      : undefined,
  );

  const feedPresence = useFeedPresence(
    typeof currentFeed?.slug === "string" ? currentFeed?.slug : undefined,
  );
  const listenerCount = feedPresence?.metas.length ?? 0;

  const playerOptions = useMemo(
    () => ({
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
    [hlsURI, currentFeed?.nodeName],
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
      } else {
        await player.play();
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
      sx={{
        minHeight: 80,
        color: "base.contrastText",
        backgroundColor: "base.main",
        mb: [8, 0],
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between",
        position: "relative",
      }}
    >
      <Box display="none">
        <VideoJS options={playerOptions} onReady={handleReady} />
      </Box>
      {(playerStatus === "playing" || playerStatus === "loading") &&
        currentFeed &&
        timestamp && (
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
      <Box mx={2}>
        <PlayPauseButton
          playerStatus={playerStatus}
          onClick={handlePlayPauseClick}
          disabled={!currentFeed}
        />
      </Box>
      <Box mx={2}>{currentFeed && <ListenerCount count={listenerCount} />}</Box>
      <Box mx={2}>
        {currentFeed
          ? `${currentFeed.name} - ${currentFeed.nodeName}`
          : "Select a location to start listening live"}
      </Box>
      <Box sx={{ mx: 4, flexGrow: 1, textAlign: "end" }}>
        {currentFeed &&
          `${currentFeed?.latLng?.lng}, ${currentFeed?.latLng?.lat}`}
      </Box>
    </Box>
  );
}
