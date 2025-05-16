import "videojs-offset";

import {
  MutableRefObject,
  ReactNode,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import { type PlayerStatus } from "@/components/Player/Player";
import { type VideoJSPlayer } from "@/components/Player/VideoJS";
import { useNowPlaying } from "@/context/NowPlayingContext";
// import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";
import { getHlsURI } from "@/hooks/useTimestampFetcher";

import { PlayerBase } from "./PlayerBase";

// // dynamically import VideoJS to speed up initial page load
// const VideoJS = dynamic(() => import("../Player/VideoJS"));

export function PlaybarPlayer({
  clipDateTime,
  clipNode,
  feed,
  image,
  marks,
  playlistTimestamp,
  startOffset,
  endOffset,
  duration,
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
  duration?: string;
  onAudioPlay?: () => void;
  onPlayerInit?: (player: VideoJSPlayer) => void;
  onPlay?: () => void;
  masterPlayerTimeRef?: MutableRefObject<number>;
}) {
  const {
    masterPlayerRef,
    masterPlayerStatus,
    setMasterPlayerStatus,
    onPlayerEnd,
  } = useNowPlaying();
  const [playerStatus, setPlayerStatus] = useState<PlayerStatus>("idle");
  const playerRef = useRef<VideoJSPlayer | null>(null);
  const [playerTime, setPlayerTime] = useState(startOffset);

  // const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  // const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));
  // const theme = useTheme();

  // const sliderMax = endOffset - startOffset;
  // const sliderValue = playerTime - startOffset;
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
      setMasterPlayerStatus("error");
    };
  }, [hlsURI, feed.nodeName, setMasterPlayerStatus, setPlayerStatus]);

  useEffect(() => {
    console.log("playerStatus: " + playerStatus);
    console.log("masterPlayerStatus: " + masterPlayerStatus);
  }, [playerStatus, masterPlayerStatus]);

  return (
    <PlayerBase
      type="candidate"
      playerOptions={playerOptions}
      startOffset={startOffset}
      endOffset={endOffset}
      handleReady={handleReady}
      playerStatus={playerStatus}
      feed={feed}
      image={image}
      playerTitle={clipDateTime}
      playerSubtitle={clipNode}
      duration={duration}
      marks={marks}
      playerRef={playerRef}
      playerTime={playerTime}
      onAudioPlay={onAudioPlay}
      handlePlayPauseClickCandidate={handlePlayPauseClick}
    />
  );
}
