import { useCallback, useMemo, useState } from "react";

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
 *
 * `onReconnect` is called when the socket has reconnected and the subscription
 * has been registered again. Updates published in between were lost, and what
 * this hook had accumulated may be stale, so it is cleared and the caller
 * should refetch the images.
 */
export function useAudioImageUpdatedSubscription(
  feedId: string,
  startTime: Date,
  endTime: Date,
  onReconnect?: () => void,
) {
  const [audioImages, setAudioImages] = useState<
    Record<string, AudioImageCreatedType | AudioImageUpdatedType>
  >({});

  // Keyed on the values, not the Date objects, so a re-render with the same
  // window does not tear the subscription down and set it up again.
  const startMs = startTime.getTime();
  const endMs = endTime.getTime();
  const query = useMemo(
    () => ({
      query: AudioImageUpdatedDocument,
      variables: {
        feedId,
        startTime: new Date(startMs),
        endTime: new Date(endMs),
      },
    }),
    [feedId, startMs, endMs],
  );

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

  const onRejoin = useCallback(() => {
    setAudioImages({});
    onReconnect?.();
  }, [onReconnect]);

  useSubscription({ query, onData, onRejoin });

  return Object.values(audioImages);
}
