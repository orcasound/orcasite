import { Channel } from "phoenix";
import { useEffect } from "react";

import { AudioImage } from "@/graphql/generated";
import socket from "@/utils/socket";

/**
 * Listens for audio image updates for a given feed (e.g. spectrogram generation)
 */
export function useAudioImageListener(
  feedId: string,
  callback: (audioImage: AudioImage) => void,
) {
  useEffect(() => {
    let channel: Channel | undefined;

    console.log("audio image listener", socket);

    if (socket) {
      const newChannel = socket.channel(`audio_image:${feedId}`, {});
      channel = newChannel;

      channel.on("updated", (payload: AudioImage) => {
        callback(payload);
      });

      newChannel.join();
    }

    return () => {
      channel?.leave();
    };
  }, [feedId, callback]);

  return;
}
