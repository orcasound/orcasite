import { Channel } from "phoenix";
import { useEffect } from "react";

import type { GraphQLDocumentString } from "@/graphql/client";

import useSocket from "./useSocket";

export function useSubscription<TData>({
  query,
  onData,
}: {
  query: { query: GraphQLDocumentString; variables: object };
  onData: (payload: {
    result: { data: TData };
    subscriptionId: string;
  }) => void;
}) {
  const socket = useSocket();

  useEffect(() => {
    let channel: Channel | undefined;
    let subscriptionChannel: Channel | undefined;
    let currentSubscriptionId: string | undefined;
    let subscribing = false;

    if (socket) {
      channel = socket.channel("__absinthe__:control");
      channel.on("phx_reply", (payload) => {
        if (
          payload?.status === "ok" &&
          currentSubscriptionId === undefined &&
          !subscribing
        ) {
          subscribing = true;
          channel?.push("doc", {
            query: query.query.toString(),
            variables: query.variables,
          });
        }

        // Subscribed to doc
        const subscriptionId = payload?.response?.subscriptionId;
        if (subscriptionId && socket) {
          currentSubscriptionId = subscriptionId;
          subscriptionChannel = socket.channel(subscriptionId);
          subscriptionChannel.on("subscription:data", onData);
          // NOTE: You don't need to join the subscriptionChannel to start
          // receiving data.
        }
      });
      channel.join();
    }

    return () => {
      channel?.leave();
      subscriptionChannel?.leave();
    };
  }, [query, onData, socket]);
}
