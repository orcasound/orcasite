import { Channel, Presence } from "phoenix";
import { useEffect, useState } from "react";

import useSocket from "./useSocket";

/**
 * Hook to use Phoenix Presence for a specific feed
 * Automatically joins and leaves the feed channel
 * @returns Portion of Presence object pertaining to the feed
 */
export default function useFeedPresence(feedId?: string) {
  const [feedPresence, setFeedPresence] = useState<Record<string, unknown[]>>();

  const socket = useSocket();

  useEffect(() => {
    let channel: Channel | undefined;

    if (socket && feedId) {
      channel = socket.channel(`feed:${feedId}`, {});

      const presence = new Presence(channel);
      presence.onSync(() => {
        const state = Object.fromEntries(
          presence.list((id, pres) => [id, pres]),
        );
        setFeedPresence(state[feedId]);
      });
      channel.join();
    }

    return () => {
      channel?.leave();
    };
  }, [feedId, socket]);

  return feedPresence;
}

export function useListenerCount(feedSlug: string) {
  const [listenerCount, setListenerCount] = useState<number>();
  const socket = useSocket();

  useEffect(() => {
    let channel: Channel | undefined;

    if (socket) {
      channel = socket.channel(`listener_counts:${feedSlug}`, {});

      channel.on("listener_counts_state", (payload) => {
        setListenerCount(payload.count || 0);
      });

      channel.join();
    }

    return () => {
      channel?.leave();
    };
  }, [socket, feedSlug]);

  return listenerCount;
}
