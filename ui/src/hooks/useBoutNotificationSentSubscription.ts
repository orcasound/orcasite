import { useCallback, useState } from "react";

import {
  BoutNotificationSentDocument,
  BoutNotificationSentSubscription,
} from "@/graphql/generated";

import { useSubscription } from "./useSubscription";

type BoutNotificationUpdatedType = NonNullable<
  BoutNotificationSentSubscription["boutNotificationSent"]
>["updated"];
/**
 * Listens for audio image updates for a given feed (e.g. spectrogram generation)
 */
export function useBoutNotificationSentSubscription(boutId: string) {
  const [notifications, setNotifications] = useState<
    Record<string, NonNullable<BoutNotificationUpdatedType>>
  >({});

  const query = {
    query: BoutNotificationSentDocument,
    variables: { boutId },
  };

  const onData = useCallback(
    (payload: {
      result: { data: BoutNotificationSentSubscription };
      subscriptionId: string;
    }) => {
      const updated = payload.result.data.boutNotificationSent?.updated;
      setNotifications((notifs) => ({
        ...notifs,
        ...(updated && { [updated.id]: updated }),
      }));
    },
    [],
  );

  useSubscription({ query, onData });

  return Object.values(notifications);
}
