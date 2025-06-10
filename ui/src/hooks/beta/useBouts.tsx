// import { useQuery } from "@tanstack/react-query";
// import { gql, request } from "graphql-request";

// import { Bout } from "@/graphql/generated";

// const endpoint = "https://live.orcasound.net/graphiql/";

// type BoutsResponse = {
//   bouts: {
//     results: Bout[];
//   };
// };

// const GET_BOUTS = gql`
//   query GetBouts($feedId: String!) {
//     bouts(feedId: $feedId) {
//       results {
//         id
//         name
//         category
//         duration
//         startTime
//         endTime
//         feedId
//       }
//     }
//   }
// `;

// export const useBouts = ({
//   feedId,
//   enabled = true,
// }: {
//   feedId: string | undefined;
//   enabled?: boolean;
// }) => {
//   return useQuery<BoutsResponse>({
//     queryKey: ["bouts", feedId],
//     queryFn: async () => {
//       if (!feedId) throw new Error("feedId is undefined");
//       return await request(endpoint, GET_BOUTS, { feedId });
//     },
//     enabled: !!feedId && enabled,
//     staleTime: 5 * 60 * 1000, // optional: cache for 5 minutes
//   });
// };

import { useQuery } from "@tanstack/react-query";
import { request } from "graphql-request";

import {
  BoutsDocument,
  BoutsQuery,
  BoutsQueryVariables,
} from "@/graphql/generated";

const endpoint = "https://live.orcasound.net/graphql";

export const useBouts = ({
  feedId,
  enabled = true,
}: {
  feedId: string | undefined;
  enabled?: boolean;
}) => {
  return useQuery<BoutsQuery>({
    queryKey: ["bouts", feedId],
    queryFn: async () => {
      if (!feedId) throw new Error("feedId is undefined");

      const variables: BoutsQueryVariables = {
        feedId,
      };

      return await request<BoutsQuery>(endpoint, BoutsDocument, variables);
    },
    enabled: !!feedId && enabled,
    staleTime: 5 * 60 * 1000,
  });
};

import {
  BoutDocument,
  BoutQuery,
  BoutQueryVariables,
} from "@/graphql/generated";

export const useBout = ({
  id,
  enabled = true,
}: {
  id: string;
  enabled?: boolean;
}) => {
  return useQuery<BoutQuery>({
    queryKey: ["bout", id],
    queryFn: async () => {
      if (!id) throw new Error("feedId is undefined");

      const variables: BoutQueryVariables = {
        id,
      };

      return await request<BoutQuery>(endpoint, BoutDocument, variables);
    },
    enabled: !!id && enabled,
    staleTime: 5 * 60 * 1000,
  });
};
