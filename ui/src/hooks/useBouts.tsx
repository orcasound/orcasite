import { useQuery } from "@tanstack/react-query";
import { gql, request } from "graphql-request";

import { Bout } from "@/graphql/generated";

const endpoint = "https://live.orcasound.net/graphiql/";

type BoutsResponse = {
  bouts: {
    results: Bout[];
  };
};

const GET_BOUTS = gql`
  query GetBouts($feedId: String!) {
    bouts(feedId: $feedId) {
      results {
        id
        name
        category
        duration
        startTime
        endTime
        feedId
      }
    }
  }
`;

export const useBouts = ({
  feedId,
  enabled = true,
}: {
  feedId: string | undefined;
  enabled?: boolean;
}) => {
  return useQuery<BoutsResponse>({
    queryKey: ["bouts", feedId],
    queryFn: async () => {
      if (!feedId) throw new Error("feedId is undefined");
      return await request(endpoint, GET_BOUTS, { feedId });
    },
    enabled: !!feedId && enabled,
    staleTime: 5 * 60 * 1000, // optional: cache for 5 minutes
  });
};
