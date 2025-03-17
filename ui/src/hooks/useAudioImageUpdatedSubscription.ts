import { Channel } from "phoenix";
import { useEffect, useState } from "react";

import {
  AudioImageUpdatedDocument,
  AudioImageUpdatedSubscription,
} from "@/graphql/generated";
import socket from "@/utils/socket";

type AudioImageCreatedType = NonNullable<
  AudioImageUpdatedSubscription["audioImageUpdated"]
>["created"];
type AudioImageUpdatedType = NonNullable<
  AudioImageUpdatedSubscription["audioImageUpdated"]
>["updated"];
/**
 * Listens for audio image updates for a given feed (e.g. spectrogram generation)
 */
export function useAudioImageUpdatedSubscription(
  feedId: string,
  startTime: Date,
  endTime: Date,
) {
  const [audioImages, setAudioImages] = useState<
    Record<string, AudioImageCreatedType | AudioImageUpdatedType>
  >({});

  useEffect(() => {
    let channel: Channel | undefined;
    let subscriptionChannel: Channel | undefined;
    let currentSubscriptionId: string | undefined;
    let subscribing = false;

    if (socket !== null) {
      const channel = socket.channel("__absinthe__:control", {});
      channel.on("phx_reply", (payload) => {
        if (
          payload?.status === "ok" &&
          currentSubscriptionId === undefined &&
          !subscribing
        ) {
          subscribing = true;
          channel.push("doc", {
            query: AudioImageUpdatedDocument,
            variables: { feedId, startTime, endTime },
          });
        }

        // Subscribed to doc
        const subscriptionId = payload?.response?.subscriptionId;
        if (subscriptionId && socket) {
          currentSubscriptionId = subscriptionId;
          subscriptionChannel = socket.channel(subscriptionId);
          subscriptionChannel.on(
            "subscription:data",
            (payload: {
              result: { data: AudioImageUpdatedSubscription };
              subscriptionId: string;
            }) => {
              const created = payload.result.data.audioImageUpdated?.created;
              const updated = payload.result.data.audioImageUpdated?.updated;
              setAudioImages((images) => ({
                ...images,
                ...(created && { [created.id]: created }),
                ...(updated && { [updated.id]: updated }),
              }));
            },
          );
        }
      });
      channel.join();
    }

    return () => {
      channel?.leave();
      subscriptionChannel?.leave();
    };
  }, [endTime, feedId, startTime]);

  return Object.values(audioImages);
}
