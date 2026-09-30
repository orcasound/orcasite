import { Channel } from "phoenix";
import { useEffect } from "react";

import useSocket from "./useSocket";

/**
 * Subscribes to an Absinthe GraphQL subscription over the Phoenix socket.
 *
 * A subscription lives in the server-side socket process, so it dies with the
 * connection. When the socket reconnects, the control channel rejoins and the
 * join push replies "ok" again; that is the moment to register the document
 * anew. Whatever was published while the connection was down was never sent,
 * so callers that keep state from the stream should refetch on `onRejoin`.
 */
export function useSubscription({
  query,
  onData,
  onRejoin,
}: {
  query: { query: string; variables: object };
  onData: (payload: {
    result: { data: { __typename?: "RootSubscriptionType" } };
    subscriptionId: string;
  }) => void;
  onRejoin?: () => void;
}) {
  const socket = useSocket();

  useEffect(() => {
    if (!socket) return;

    let subscriptionChannel: Channel | undefined;
    let joins = 0;
    let cancelled = false;

    const subscribe = () => {
      // Any subscription from before a reconnect went down with the old
      // connection; drop it before asking for a new one.
      subscriptionChannel?.leave();
      subscriptionChannel = undefined;

      control
        .push("doc", query)
        .receive("ok", ({ subscriptionId }: { subscriptionId: string }) => {
          // A reply can land after this effect was cleaned up, or after a
          // later rejoin already asked again; neither may leave a channel
          // behind that nothing will close.
          if (cancelled) return;
          subscriptionChannel?.leave();
          subscriptionChannel = socket.channel(subscriptionId);
          subscriptionChannel.on("subscription:data", onData);
          // NOTE: You don't need to join the subscriptionChannel to start
          // receiving data.
        });
    };

    const control = socket.channel("__absinthe__:control");
    control.join().receive("ok", () => {
      joins += 1;
      subscribe();
      if (joins > 1) onRejoin?.();
    });

    return () => {
      cancelled = true;
      control.leave();
      subscriptionChannel?.leave();
    };
  }, [query, onData, onRejoin, socket]);
}
