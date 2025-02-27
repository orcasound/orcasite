import { Channel } from "phoenix";
import { useEffect, useState } from "react";

import { AudioImage } from "@/graphql/generated";
import socket from "@/utils/socket";

/**
 * Listens for audio image updates for a given feed (e.g. spectrogram generation)
 */
export default function useAudioImageListener(feedSlug?: string) {
  const [audioImages, setAudioImages] = useState<Record<string, AudioImage>>(
    {},
  );

  useEffect(() => {
    let channel: Channel | undefined;

    if (socket) {
      const newChannel = socket.channel(`audio_images:${feedSlug}`, {});
      channel = newChannel;

      channel.on("audio_image_update", (payload) => {
        setAudioImages({ ...audioImages, [payload.id]: payload });
      });

      newChannel.join();
    }

    return () => {
      channel?.leave();
    };
  }, [feedSlug]);

  return audioImages;
}
