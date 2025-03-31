import { useCallback, useState } from "react";

import {
  AudioImageUpdatedDocument,
  AudioImageUpdatedSubscription,
} from "@/graphql/generated";

import { useSubscription } from "./useSubscription";

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

  const query = {
    query: AudioImageUpdatedDocument,
    variables: { feedId, startTime, endTime },
  };

  const onData = useCallback(
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
    [],
  );

  useSubscription({ query, onData });

  return Object.values(audioImages);
}
