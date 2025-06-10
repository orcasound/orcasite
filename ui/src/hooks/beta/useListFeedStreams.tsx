import { useQuery } from "@tanstack/react-query";
import { request } from "graphql-request";

import {
  ListFeedStreamsDocument,
  ListFeedStreamsQuery,
  ListFeedStreamsQueryVariables,
} from "@/graphql/generated";

const endpoint = "https://live.orcasound.net/graphql";

export const useListFeedStreams = ({
  feedId,
  fromDateTime,
  toDateTime,
  dayBeforeFromDateTime,
  enabled = true,
}: {
  feedId: string | undefined;
  fromDateTime: Date;
  toDateTime: Date;
  dayBeforeFromDateTime: Date;
  enabled?: boolean;
}) => {
  return useQuery<ListFeedStreamsQuery>({
    queryKey: ["feedstreams", feedId, fromDateTime, toDateTime],
    queryFn: async () => {
      if (!feedId) throw new Error("feedId is undefined");

      const variables: ListFeedStreamsQueryVariables = {
        feedId,
        fromDateTime,
        toDateTime,
        dayBeforeFromDateTime,
      };

      return await request<ListFeedStreamsQuery>(
        endpoint,
        ListFeedStreamsDocument,
        variables,
      );
    },
    enabled: !!feedId && enabled,
  });
};
