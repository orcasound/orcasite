import { Box, Typography } from "@mui/material";
import dynamic from "next/dynamic";
import { useCallback, useMemo, useRef, useState } from "react";

import { type PlayerStatus } from "./Player";
import PlayPauseButton from "./PlayPauseButton";
import { type VideoJSPlayer } from "./VideoJS";

const VideoJS = dynamic(() => import("./VideoJS"));

const playerOffsetToDateTime = (playlistDatetime: Date, playerOffset: number) =>
  new Date(playlistDatetime.valueOf() + playerOffset * 1000);

export function BoutPlayer({
  onPlayerTimeUpdate,
}: {
  onPlayerTimeUpdate?: (time: Date) => void;
}) {
  const playlistTimestamp = "1732665619";
  const playlistDatetime = new Date(Number(playlistTimestamp) * 1000);
  const hlsURI = `https://audio-orcasound-net.s3.amazonaws.com/rpi_port_townsend/hls/${playlistTimestamp}/live.m3u8`;
  const now = useMemo(() => new Date(), []);
  const [playerStatus, setPlayerStatus] = useState<PlayerStatus>("idle");
  const playerRef = useRef<VideoJSPlayer | null>(null);
  const [playerOffset, setPlayerOffset] = useState<number>(
    now.valueOf() / 1000 - Number(playlistTimestamp),
  );

  const playerDateTime = useMemo(
    () => playerOffsetToDateTime(playlistDatetime, playerOffset),
    [playlistDatetime, playerOffset],
  );

  const playerOptions = useMemo(
    () => ({
      autoplay: false,
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
      sources: [
        {
          // If hlsURI isn't set, use a dummy URI to trigger an error
          // The dummy URI doesn't actually exist, it should return 404
          // This is the only way to get videojs to throw an error, otherwise
          // it just won't initialize (if src is undefined/null/empty))
          src: hlsURI,
          type: "application/x-mpegurl",
        },
      ],
    }),
    [hlsURI], //, feed?.nodeName],
  );

  const handleReady = useCallback(
    (player: VideoJSPlayer) => {
      playerRef.current = player;

      player.on("playing", () => {
        setPlayerStatus("playing");
        // const currentTime = player.currentTime() ?? 0;
        // if (currentTime < startOffset || currentTime > endOffset) {
        //   player.currentTime(startOffset);
        //   setPlayerOffset(endOffset);
        // }
      });
      player.on("pause", () => setPlayerStatus("paused"));
      player.on("waiting", () => setPlayerStatus("loading"));
      player.on("error", () => setPlayerStatus("error"));
      // player.currentTime(startOffset);

      player.on("timeupdate", () => {
        const currentTime = player.currentTime() ?? 0;
        // if (currentTime > endOffset) {
        //   player.currentTime(startOffset);
        //   setPlayerOffset(startOffset);
        // } else {
        //   setPlayerTime(currentTime);
        // }
        setPlayerOffset(currentTime);
        if (onPlayerTimeUpdate !== undefined) {
          onPlayerTimeUpdate(
            playerOffsetToDateTime(playlistDatetime, currentTime),
          );
        }
      });
    },
    // [startOffset, endOffset],
    [onPlayerTimeUpdate],
  );
  const handlePlayPauseClick = () => {
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
        player.pause();
      } else {
        player.play();
      }
    } catch (e) {
      console.error(e);
      // AbortError is thrown if pause() is called while play() is still loading (e.g. if segments are 404ing)
      // It's not important, so don't show this error to the user
      if (e instanceof DOMException && e.name === "AbortError") return;
      setPlayerStatus("error");
    }
  };
  return (
    <div>
      <Box display="none">
        <VideoJS options={playerOptions} onReady={handleReady} />
      </Box>
      <Box ml={2} mr={6} display="flex" justifyContent="center">
        <Box
          display="flex"
          flexDirection="column"
          justifyContent="center"
          alignItems="center"
        >
          <Box mb={1}>
            <PlayPauseButton
              playerStatus={playerStatus}
              onClick={handlePlayPauseClick}
              disabled={false}
            />
          </Box>
          <Typography variant="body1" fontWeight={"bold"}>
            {playerDateTime !== undefined &&
              playerDateTime.toLocaleTimeString()}
          </Typography>
          <Typography variant="subtitle2">
            {playerDateTime !== undefined &&
              playerDateTime.toLocaleDateString()}
          </Typography>
        </Box>
      </Box>
    </div>
  );
}
