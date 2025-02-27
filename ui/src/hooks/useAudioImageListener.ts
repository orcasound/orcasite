import { Channel } from "phoenix";
import { useEffect } from "react";

import { AudioImage } from "@/graphql/generated";
import socket from "@/utils/socket";

/**
 * Listens for audio image updates for a given feed (e.g. spectrogram generation)
 */
function useAudioImageListener(
  feedSlug: string,
  callback: (audioImage: AudioImage) => void,
) {
  useEffect(() => {
    let channel: Channel | undefined;

    if (socket) {
      const newChannel = socket.channel(`audio_images:${feedSlug}`, {});
      channel = newChannel;

      channel.on("audio_image_update", (payload: AudioImage) => {
        callback(payload);
      });

      newChannel.join();
    }

    return () => {
      channel?.leave();
    };
  }, [feedSlug, callback]);

  return;
}
