import { useCallback, useMemo, useState } from "react";

import {
  BoutNotificationSentDocument,
  BoutNotificationSentSubscription,
} from "@/graphql/generated";

import { useSubscription } from "./useSubscription";

type BoutNotificationUpdatedType = NonNullable<
  BoutNotificationSentSubscription["boutNotificationSent"]
>["updated"];
/**
 * Listens for notifications sent for a bout.
 *
 * `onReconnect` is called when the socket has reconnected and the subscription
 * has been registered again; notifications sent in between were lost, so what
 * this hook had accumulated is cleared and the caller should refetch.
 */
export function useBoutNotificationSentSubscription(
  boutId: string,
  onReconnect?: () => void,
) {
  const [notifications, setNotifications] = useState<
    Record<string, NonNullable<BoutNotificationUpdatedType>>
  >({});

  const query = useMemo(
    () => ({
      query: BoutNotificationSentDocument,
      variables: { boutId },
    }),
    [boutId],
  );

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

  const onRejoin = useCallback(() => {
    setNotifications({});
    onReconnect?.();
  }, [onReconnect]);

  useSubscription({ query, onData, onRejoin });

  return Object.values(notifications);
}
