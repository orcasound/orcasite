import { Channel, Presence } from "phoenix";
import { useEffect, useState } from "react";

import socket from "@/utils/socket";

/**
 * Hook to use Phoenix Presence for a specific feed
 * Automatically joins and leaves the feed channel
 * @returns Portion of Presence object pertaining to the feed
 */
export default function useFeedPresence(feedId?: string) {
  const [feedPresence, setFeedPresence] = useState<Record<string, unknown[]>>();

  useEffect(() => {
    let channel: Channel | undefined;

    if (socket && feedId) {
      const newChannel = socket.channel(`feed:${feedId}`, {});
      channel = newChannel;

      const presence = new Presence(newChannel);
      presence.onSync(() => {
        const state = Object.fromEntries(
          presence.list((id, pres) => [id, pres]),
        );
        setFeedPresence(state[feedId]);
      });
      newChannel.join();
    }

    return () => {
      channel?.leave();
    };
  }, [feedId]);

  return feedPresence;
}

export function useListenerCount(feedSlug: string) {
  const [listenerCount, setListenerCount] = useState<number>();

  useEffect(() => {
    let channel: Channel | undefined;

    if (socket) {
      const newChannel = socket.channel(`listener_counts:${feedSlug}`, {});
      channel = newChannel;

      channel.on("listener_counts_state", (payload) => {
        setListenerCount(payload.count || 0);
      });

      newChannel.join();
    }

    return () => {
      channel?.leave();
    };
  }, [feedSlug]);

  return listenerCount;
}
