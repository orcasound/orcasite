import "videojs-offset";

import {
  Box,
  Slider,
  Stack,
  Theme,
  Typography,
  useMediaQuery,
  useTheme,
} from "@mui/material";
import dynamic from "next/dynamic";
import {
  MutableRefObject,
  ReactNode,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import { useNowPlaying } from "@/context/NowPlayingContext";
// import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";
import { getHlsURI } from "@/hooks/useTimestampFetcher";

import PlayBarPlayPauseButton from "./PlayBarPlayPauseButton";
import { type PlayerStatus } from "./Player";
import { type VideoJSPlayer } from "./VideoJS";

// dynamically import VideoJS to speed up initial page load

const VideoJS = dynamic(() => import("./VideoJS"));

export function PlaybarPlayer({
  clipDateTime,
  clipNode,
  feed,
  image,
  marks,
  playlistTimestamp,
  startOffset,
  endOffset,
  onAudioPlay,
  onPlayerInit,
  onPlay,
  masterPlayerTimeRef,
}: {
  clipDateTime?: string;
  clipNode?: string;
  feed: Feed;
  image: string | undefined;
  marks?: { label: string | ReactNode; value: number }[];
  playlistTimestamp: number;
  startOffset: number;
  endOffset: number;
  onAudioPlay?: () => void;
  onPlayerInit?: (player: VideoJSPlayer) => void;
  onPlay?: () => void;
  masterPlayerTimeRef?: MutableRefObject<number>;
}) {
  const { masterPlayerRef, setMasterPlayerStatus, onPlayerEnd } =
    useNowPlaying();
  const [playerStatus, setPlayerStatus] = useState<PlayerStatus>("idle");
  const playerRef = useRef<VideoJSPlayer | null>(null);
  const [playerTime, setPlayerTime] = useState(startOffset);

  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  // const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));
  const theme = useTheme();

  const sliderMax = endOffset - startOffset;
  const sliderValue = playerTime - startOffset;
  // const sliderValue = playerTimeRef.current - startOffset;

  const hlsURI = getHlsURI(feed.bucket, feed.nodeName, playlistTimestamp);

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
      // auto-play the player when it mounts -- mounting is triggered in Playbar based on nowPlaying
      if (playerRef.current) {
        masterPlayerRef.current = playerRef.current;
        player.play();
      }
      if (onPlayerInit) onPlayerInit(player);
      player.on("playing", () => {
        setPlayerStatus("playing");
        setMasterPlayerStatus("playing");
        const currentTime = player.currentTime() ?? 0;
        if (currentTime < startOffset || currentTime > endOffset) {
          player.currentTime(startOffset);
        }
        if (onPlay) onPlay();
        // if (setNowPlaying && candidate) setNowPlaying(candidate);
      });
      player.on("pause", () => {
        setPlayerStatus("paused");
        setMasterPlayerStatus("paused");
      });
      player.on("waiting", () => {
        setPlayerStatus("loading");
        setMasterPlayerStatus("loading");
      });
      player.on("error", () => {
        setPlayerStatus("error");
        setMasterPlayerStatus("error");
      });

      player.on("timeupdate", () => {
        const currentTime = player.currentTime() ?? 0;
        if (currentTime > endOffset) {
          player.currentTime(startOffset);
          if (masterPlayerTimeRef) masterPlayerTimeRef.current = startOffset;
          setPlayerTime(startOffset);
          player.pause();
          if (onPlayerEnd) onPlayerEnd();
        } else {
          setPlayerTime(currentTime);
          if (masterPlayerTimeRef) masterPlayerTimeRef.current = currentTime;
        }
      });
      player.on("loadedmetadata", () => {
        // On initial load, set player time to startOffset
        player.currentTime(startOffset);
      });
    },
    [
      startOffset,
      endOffset,
      onPlay,
      onPlayerEnd,
      onPlayerInit,
      masterPlayerRef,
      setMasterPlayerStatus,
      masterPlayerTimeRef,
    ],
  );

  const handlePlayPauseClick = () => {
    const player = playerRef.current;

    if (playerStatus === "error") {
      setPlayerStatus("idle");
      setMasterPlayerStatus("idle");
      return;
    }

    if (!player) {
      setPlayerStatus("error");
      setMasterPlayerStatus("error");
      return;
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
      setMasterPlayerStatus("error");
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

  useEffect(() => {
    console.log("playerStatus: " + playerStatus);
  }, [playerStatus]);

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

  return (
    <Box
      sx={(theme) => ({
        minHeight: theme.spacing(10),
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between",
        px: [0, 2],
        position: "relative",
        // className: "candidate-card-player",
        // Keep player above the sliding drawer
        zIndex: theme.zIndex.drawer + 1,
        width: "100%",
        flexFlow: smDown ? "row-reverse" : "row",
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
        <PlayBarPlayPauseButton
          playerStatus={playerStatus}
          onClick={handlePlayPauseClick}
          disabled={!feed}
        />
      </Box>
      <Stack
        direction="row"
        width="100%"
        spacing={smDown ? 2 : 3}
        sx={{ overflow: "visible" }}
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
            sx={{ whiteSpace: "nowrap", fontSize: smDown ? "14px" : "1rem" }}
          >
            <span style={{ fontWeight: "bold" }}>{clipDateTime}</span>
            {smDown ? <br /> : " • "}
            {clipNode}
          </Typography>

          <Box
            width={"100%"}
            // height={"42px"}
            id="slider"
            sx={{ display: smDown ? "none" : "block" }}
          >
            <Slider
              // valueLabelDisplay="auto"
              // valueLabelFormat={(v) => `${formattedSeconds(
              //   Number((v).toFixed(0)),
              // )}`}
              step={0.1}
              max={sliderMax}
              value={sliderValue}
              marks={marks}
              onChange={handleSliderChange}
              onChangeCommitted={handleSliderChangeCommitted}
              size="small"
              sx={{
                padding: "10px 0!important",
                display: "flex",
                flexDirection: "column",
                gap: 0,
                "& .MuiSlider-markLabel": {
                  display: "block",
                  top: 0,
                },
                "& .MuiSlider-mark": {
                  backgroundColor: theme.palette.accent3.main,
                  height: "10px",
                },
                "&.MuiSlider-root": {
                  marginBottom: "0px",
                },
              }}
            />
            <Box
              id="formatted-seconds"
              sx={{ display: "flex", justifyContent: "space-between" }}
            >
              <Typography component="p" variant="subtitle2">
                {formattedSeconds(
                  Number((playerTime - startOffset).toFixed(0)),
                )}
              </Typography>
              <Typography component="p" variant="subtitle2">
                {"-" +
                  formattedSeconds(Number((endOffset - playerTime).toFixed(0)))}
              </Typography>
            </Box>
          </Box>
        </Box>
      </Stack>
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
