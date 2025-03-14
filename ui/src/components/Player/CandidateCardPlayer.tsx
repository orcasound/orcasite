import "videojs-offset";

import { Box, Slider, Typography } from "@mui/material";
import dynamic from "next/dynamic";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";

import { Feed } from "@/graphql/generated";
import { getHlsURI } from "@/hooks/useTimestampFetcher";
import { mobileOnly } from "@/styles/responsive";

import { type PlayerStatus } from "./Player";
import PlayPauseButton from "./PlayPauseButton";
import { type VideoJSPlayer } from "./VideoJS";

// dynamically import VideoJS to speed up initial page load

const VideoJS = dynamic(() => import("./VideoJS"));

export function CandidateCardPlayer({
  feed,
  marks,
  playlist,
  startOffset,
  endOffset,
  onAudioPlay,
  onPlayerInit,
  onPlay,
  onPlayerEnd,
  // candidate
}: {
  feed: Pick<Feed, "nodeName" | "bucket">;
  marks?: { label: string; value: number }[];
  playlist: number;
  startOffset: number;
  endOffset: number;
  onAudioPlay?: () => void;
  changeListState?: (value: number, status: string) => void;
  index?: number;
  command?: string;
  onPlayerInit?: (player: VideoJSPlayer) => void;
  onPlay?: () => void;
  onPlayerEnd?: () => void;
  // candidate?: Candidate2;
}) {
  const [playerStatus, setPlayerStatus] = useState<PlayerStatus>("idle");
  const playerRef = useRef<VideoJSPlayer | null>(null);
  const [playerTime, setPlayerTime] = useState(startOffset);
  // const { setNowPlaying } = useNowPlaying();

  console.log("initial render: startOffset", startOffset);
  console.log("initial render: playerTime", playerTime);

  const startOffsetRef = useRef(startOffset);

  useEffect(() => {
    startOffsetRef.current = startOffset;
  }, [startOffset]);

  const sliderValue = playerTime - startOffsetRef.current;
  const sliderMax = endOffset - startOffsetRef.current;
  const hlsURI = getHlsURI(feed.bucket, feed.nodeName, playlist);

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
          src: hlsURI ?? `${feed.nodeName}/404`,
          type: "application/x-mpegurl",
        },
      ],
    }),
    [hlsURI, feed?.nodeName],
  );

  const handleReady = useCallback(
    (player: VideoJSPlayer) => {
      playerRef.current = player;
      onPlayerInit?.(player);
      setPlayerTime(startOffset);

      let metadataLoaded = false; // Flag to ensure metadata is loaded before timeupdate

      console.log("handleReady: startOffset", startOffset);
      // console.log("handleReady: playerTime", playerTime)

      player.on("playing", () => {
        setPlayerStatus("playing");
        if (!metadataLoaded) {
          console.log("Forcing seek to startOffset:", startOffset);
          player.currentTime(startOffset);
        }

        const currentTime = player.currentTime() ?? 0;
        if (currentTime < startOffset || currentTime > endOffset) {
          player.currentTime(startOffset);
        }
        onPlay?.();
        // candidate && setNowPlaying(candidate)
      });
      player.on("pause", () => {
        setPlayerStatus("paused");
      });
      player.on("waiting", () => setPlayerStatus("loading"));
      player.on("error", () => setPlayerStatus("error"));

      player.on("timeupdate", () => {
        if (!metadataLoaded) return; // Ignore timeupdate events before metadata loads
        const currentTime = player.currentTime() ?? 0;
        const currentTimePlus =
          currentTime !== startOffsetRef.current
            ? startOffsetRef.current
            : currentTime;
        if (currentTime > endOffset) {
          player.currentTime(startOffsetRef.current);
          setPlayerTime(startOffsetRef.current);
          player.pause();
          onPlayerEnd?.();
        } else {
          setPlayerTime(currentTime);
          console.log(
            "Timeupdate currentTime <= endOffset: currentTime =",
            currentTime,
          );
          console.log(
            "Timeupdate currentTime <= endOffset: startOffsetRef.current =",
            startOffsetRef.current,
          );
          console.log(
            "Timeupdate currentTime <= endOffset: currentTimePlus =",
            currentTimePlus,
          );
          console.log(
            "Timeupdate currentTime <= endOffset: startOffset =",
            startOffset,
          );
          console.log(
            "Timeupdate currentTime <= endOffset: endOffset =",
            endOffset,
          );
        }
      });
      player.on("loadedmetadata", () => {
        console.log("Loaded Metadata - stale startOffset:", startOffset);
        console.log(
          "Loaded Metadata - startOffsetRef.current:",
          startOffsetRef.current,
        );
        console.log(
          "loadedmetadata event fired - player.currentTime:",
          player.currentTime(),
        );
        // console.log("loadedmetadata event fired - playerTime:", playerTime);
        // On initial load, set player time to startOffset
        metadataLoaded = true; // Set flag to allow timeupdate
        player.currentTime(startOffsetRef.current);
        // setPlayerTime(startOffset); // Ensure UI updates
      });
    },
    [
      startOffset,
      endOffset,
      onPlayerInit, // memoized inside useCallback in CandidateCard
      onPlay, // memoized inside useCallback in CandidateCard
      onPlayerEnd, // memoized inside useCallback in CandidateCard
    ],
  );

  useEffect(() => {
    console.log("Syncing playerTime with startOffset:", startOffset);
    setPlayerTime(startOffset);
    if (playerRef.current) {
      playerRef.current.currentTime(startOffset);
    }
  }, [startOffset]);

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

    if (playerStatus !== "playing") {
      console.log(
        "clicked play and setPlayerTime(startOffsetRef.current)",
        startOffsetRef.current,
      );
      setPlayerTime(startOffsetRef.current);
    }

    try {
      if (playerStatus === "loading" || playerStatus === "playing") {
        player.pause();
      } else {
        player.play();
        onAudioPlay?.();
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
  }, [hlsURI, feed.nodeName]);

  const handleSliderChange = (
    _e: Event,
    v: number | number[],
    _activeThumb: number,
  ) => {
    playerRef?.current?.pause();
    if (typeof v !== "number") return;
    playerRef?.current?.currentTime(v + startOffset);
  };

  const handleSliderChangeCommitted = (
    _e: Event | React.SyntheticEvent<Element, Event>,
    v: number | number[],
  ) => {
    if (typeof v !== "number") return;
    playerRef?.current?.currentTime(v + startOffset);
    playerRef?.current?.play();
  };

  const playerTimePlus =
    playerTime < startOffsetRef.current || playerTime > endOffset
      ? startOffsetRef.current
      : playerTime;

  return (
    <Box
      sx={(theme) => ({
        minHeight: theme.spacing(10),
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
      <Box display="none" id="video-js">
        <VideoJS options={playerOptions} onReady={handleReady} />
      </Box>
      <Box ml={2} mr={6} id="play-pause-button">
        <PlayPauseButton
          playerStatus={playerStatus}
          onClick={handlePlayPauseClick}
          disabled={!feed}
        />
      </Box>
      <Box sx={{ display: "flex", flexDirection: "column", width: "90px" }}>
        <Box width={"100%"} id="slider">
          <Slider
            valueLabelDisplay="auto"
            valueLabelFormat={(v) => `${(v + startOffset).toFixed(2)} s`}
            step={0.1}
            max={sliderMax}
            value={sliderValue}
            marks={marks}
            onChange={handleSliderChange}
            onChangeCommitted={handleSliderChangeCommitted}
            size="small"
          />
        </Box>

        <Box
          id="formatted-seconds"
          sx={{ display: "flex", justifyContent: "space-between" }}
        >
          <Typography component="p" variant="subtitle2">
            {formattedSeconds(
              Number((playerTimePlus - startOffsetRef.current).toFixed(0)),
            )}
          </Typography>
          <Typography component="p" variant="subtitle2">
            {"-" +
              formattedSeconds(Number((endOffset - playerTimePlus).toFixed(0)))}
          </Typography>
          {(() => {
            console.log(
              "Rendered: playerTime",
              playerTime,
              "Rendered: playerTimePlus",
              playerTimePlus,
              "startOffset",
              startOffset,
              "startOffsetRef.current",
              startOffsetRef.current,
              "Displayed Left:",
              formattedSeconds(
                Number((playerTimePlus - startOffsetRef.current).toFixed(0)),
              ),
              "Displayed Right:",
              formattedSeconds(Number((endOffset - playerTimePlus).toFixed(0))),
              "endOffset",
              endOffset,
            );
            return <Box></Box>;
          })()}
        </Box>
      </Box>
    </Box>
  );
}

const formattedSeconds = (seconds: number) => {
  const mm = Math.floor(seconds / 60);
  const ss = seconds % 60;
  return `${Number(mm).toString().padStart(2, "0")}:${ss
    .toFixed(0)
    .padStart(2, "0")}`;
};
