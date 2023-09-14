import "videojs-offset";
import "video.js/dist/video-js.css";

import React from "react";
import videojs from "video.js";
import type Player from "video.js/dist/types/player";

import { Maybe } from "@/graphql/generated";

export type VideoJSPlayer = Player;
export type VideoJSOptions = {
  autoplay?: boolean;
  sources?: {
    src: string;
    type: string;
  }[];
  playsinline?: boolean;
  poster?: Maybe<string> | undefined;
  [key: string]: unknown;
};

// adapted from https://videojs.com/guides/react/
export default function VideoJS({
  options,
  onReady,
}: {
  options: VideoJSOptions;
  onReady?: (player: Player) => void;
}) {
  const videoRef = React.useRef<HTMLDivElement | null>(null);
  const playerRef = React.useRef<Player | null>(null);

  React.useEffect(() => {
    if (!videoRef.current) {
      console.error("no videoRef set");
      return;
    }
    // Make sure Video.js player is only initialized once
    if (!playerRef.current) {
      // The Video.js player needs to be _inside_ the component el for React 18 Strict Mode.
      const videoElement = document.createElement("video-js");

      // videoElement.classList.add('vjs-big-play-centered')
      videoRef.current.appendChild(videoElement);

      const player = (playerRef.current = videojs(videoElement, options, () => {
        videojs.log("player is ready");
        onReady && onReady(player);
      }));

      // You could update an existing player in the `else` block here
      // on prop change, for example:
    } else {
      const player = playerRef.current;

      player.autoplay(options.autoplay);
      player.src(options.sources);
      player.playsinline("playsinline" in options ? options.playsinline : true);
      if (options.poster) {
        player.poster(options.poster);
      }
    }
  }, [options, videoRef, onReady]);

  // Dispose the Video.js player when the functional component unmounts
  React.useEffect(() => {
    const player = playerRef.current;

    return () => {
      if (player && !player.isDisposed()) {
        player.dispose();
        playerRef.current = null;
      }
    };
  }, [playerRef]);

  return (
    <div data-vjs-player>
      <div ref={videoRef} />
    </div>
  );
}
