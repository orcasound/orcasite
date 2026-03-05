import { Box, IconButton, Slider, Typography } from "@mui/material";
import dynamic from "next/dynamic";
import React, {
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import { type VideoJSPlayer } from "@/components/Player/VideoJS";

const VideoJS = dynamic(() => import("@/components/Player/VideoJS"));

interface ShipnoiseDetectionsPlayerProps {
  hlsUrl: string;
  startOffsetSec: number;
  endOffsetSec: number;
  timestamp?: string | null;
  date?: string;
}

const PLAY_BUTTON_SIZE = 48;

const formattedSeconds = (seconds: number) => {
  const mm = Math.floor(seconds / 60);
  const ss = seconds % 60;
  return `${mm.toString().padStart(2, "0")}:${ss.toFixed(0).padStart(2, "0")}`;
};

const ShipnoiseDetectionsPlayer: React.FC<ShipnoiseDetectionsPlayerProps> = ({
  hlsUrl,
  startOffsetSec,
  endOffsetSec,
  timestamp,
  date,
}) => {
  const playerRef = useRef<VideoJSPlayer | null>(null);
  const [isPlaying, setIsPlaying] = useState(false);
  const [playerTime, setPlayerTime] = useState(startOffsetSec);

  const sliderMax = endOffsetSec - startOffsetSec;
  const sliderValue = playerTime - startOffsetSec;

  const marks = useMemo(() => {
    const result: { value: number; label: string }[] = [];
    const totalSec = endOffsetSec - startOffsetSec;
    for (let m = 1; m * 60 < totalSec; m++) {
      result.push({ value: m * 60, label: String(m) });
    }
    return result;
  }, [startOffsetSec, endOffsetSec]);

  const videoJsOptions = useMemo(
    () => ({
      autoplay: false,
      sources: [{ src: hlsUrl, type: "application/x-mpegURL" }],
    }),
    [hlsUrl],
  );

  useEffect(() => {
    setPlayerTime(startOffsetSec);
    setIsPlaying(false);
  }, [hlsUrl, startOffsetSec]);

  const handlePlayerReady = useCallback(
    (player: VideoJSPlayer) => {
      playerRef.current = player;

      player.on("playing", () => {
        setIsPlaying(true);
        const t = player.currentTime() ?? 0;
        if (t < startOffsetSec || t > endOffsetSec) {
          player.currentTime(startOffsetSec);
          setPlayerTime(startOffsetSec);
        }
      });

      player.on("pause", () => setIsPlaying(false));
      player.currentTime(startOffsetSec);

      player.on("timeupdate", () => {
        const t = player.currentTime() ?? 0;
        if (t > endOffsetSec) {
          player.currentTime(startOffsetSec);
          setPlayerTime(startOffsetSec);
        } else {
          setPlayerTime(t);
        }
      });
    },
    [startOffsetSec, endOffsetSec],
  );

  const handlePlayPauseClick = useCallback(() => {
    const player = playerRef.current;
    if (!player) return;

    if (isPlaying) {
      player.pause();
    } else {
      player.play()?.catch(() => {});
    }
  }, [isPlaying]);

  const handleSliderChange = useCallback(
    (_e: Event, v: number | number[]) => {
      const player = playerRef.current;
      player?.pause();
      if (typeof v !== "number") return;
      player?.currentTime(v + startOffsetSec);
      setPlayerTime(v + startOffsetSec);
    },
    [startOffsetSec],
  );

  const handleSliderChangeCommitted = useCallback(
    (
      _e: Event | React.SyntheticEvent<Element, Event>,
      v: number | number[],
    ) => {
      if (typeof v !== "number") return;
      const player = playerRef.current;
      player?.currentTime(v + startOffsetSec);
      player?.play()?.catch(() => {});
    },
    [startOffsetSec],
  );

  const formattedDateTime = useMemo(() => {
    const pacificDate = new Intl.DateTimeFormat("en-US", {
      month: "long",
      day: "numeric",
      year: "numeric",
      timeZone: "America/Los_Angeles",
    });
    const pacificTime = new Intl.DateTimeFormat("en-US", {
      hour: "numeric",
      minute: "2-digit",
      hour12: true,
      timeZone: "America/Los_Angeles",
      timeZoneName: "short",
    });

    if (timestamp) {
      const parsed = new Date(timestamp);
      if (!Number.isNaN(parsed.getTime())) {
        return `${pacificDate.format(parsed)} | ${pacificTime.format(parsed)}`;
      }
    }
    return date ?? "";
  }, [timestamp, date]);

  return (
    <Box
      sx={{
        minHeight: 80,
        display: "flex",
        alignItems: "center",
        width: "100%",
      }}
    >
      <Box display="none">
        <VideoJS options={videoJsOptions} onReady={handlePlayerReady} />
      </Box>

      <Box sx={{ ml: { xs: 0, md: 2 }, mr: { xs: 2, md: 6 } }}>
        <IconButton
          onClick={handlePlayPauseClick}
          sx={{
            width: PLAY_BUTTON_SIZE,
            height: PLAY_BUTTON_SIZE,
            borderRadius: "999px",
            p: 0,
          }}
        >
          {isPlaying ? (
            <Box
              component="svg"
              width={PLAY_BUTTON_SIZE}
              height={PLAY_BUTTON_SIZE}
              viewBox="0 0 24 24"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <circle cx="12" cy="12" r="12" fill="#002447" />
              <path d="M8 6h3v12H8V6zm5 0h3v12h-3V6z" fill="white" />
            </Box>
          ) : (
            // eslint-disable-next-line @next/next/no-img-element
            <img
              src="/shipnoise/playbutton.svg"
              alt="Play"
              width={PLAY_BUTTON_SIZE}
              height={PLAY_BUTTON_SIZE}
            />
          )}
        </IconButton>
      </Box>

      <Box sx={{ display: "flex", flexDirection: "column", width: 1 }}>
        {formattedDateTime && (
          <Typography sx={{ fontSize: "14px", color: "#4b5563", mb: 0.5 }}>
            {formattedDateTime}
          </Typography>
        )}

        <Box width="100%">
          <Slider
            valueLabelDisplay="auto"
            valueLabelFormat={(v) => `${(v + startOffsetSec).toFixed(1)} s`}
            step={0.1}
            max={sliderMax}
            value={sliderValue}
            marks={marks}
            onChange={handleSliderChange}
            onChangeCommitted={handleSliderChangeCommitted}
            aria-label="Playback position"
            sx={{ color: "#002447" }}
          />
        </Box>

        <Box sx={{ display: "flex", justifyContent: "space-between" }}>
          <Typography sx={{ fontSize: "14px", color: "#6b7280" }}>
            {formattedSeconds(Number((playerTime - startOffsetSec).toFixed(0)))}
          </Typography>
          <Typography sx={{ fontSize: "14px", color: "#6b7280" }}>
            {formattedSeconds(
              Number((endOffsetSec - startOffsetSec).toFixed(0)),
            )}
          </Typography>
        </Box>
      </Box>
    </Box>
  );
};

export default ShipnoiseDetectionsPlayer;
