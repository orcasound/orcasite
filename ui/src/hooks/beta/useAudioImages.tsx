// import { useQuery } from "@tanstack/react-query";
// import { gql, request } from "graphql-request";

// import { AudioImage } from "@/graphql/generated";

// const endpoint = "https://live.orcasound.net/graphiql/";

// type AudioImagesResponse = {
//   audioImages: {
//     hasNextPage: boolean;
//     results: AudioImage[];
//   };
// };

// const GET_AUDIO_IMAGES = gql`
//   query GetAudioImages(
//     $feedId: String!
//     $startTime: DateTime!
//     $endTime: DateTime!
//   ) {
//     audioImages(
//       feedId: $feedId
//       startTime: $startTime
//       endTime: $endTime
//       filter: { status: { notEq: "FAILED" } }
//     ) {
//       results {
//         id
//         startTime
//         endTime
//         status
//         objectPath
//         bucket
//         bucketRegion
//         feedId
//         imageSize
//         imageType
//       }
//     }
//   }
// `;

// export const useAudioImages = ({
//   feedId,
//   startTime,
//   endTime,
//   enabled = true,
// }: {
//   feedId: string | undefined;
//   startTime: Date;
//   endTime: Date;
//   enabled?: boolean;
// }) => {
//   return useQuery<AudioImagesResponse>({
//     queryKey: ["audioImages", feedId, startTime, endTime],
//     queryFn: async () => {
//       if (!feedId) throw new Error("feedId is undefined");
//       return await request(endpoint, GET_AUDIO_IMAGES, {
//         feedId,
//         startTime,
//         endTime,
//       });
//     },
//     enabled: !!feedId && enabled,
//     staleTime: 5 * 60 * 1000, // optional: cache for 5 minutes
//   });
// };

import { useQuery } from "@tanstack/react-query";
import { request } from "graphql-request";

import {
  AudioImagesDocument,
  AudioImagesQuery,
  AudioImagesQueryVariables,
} from "@/graphql/generated";

const endpoint = "https://live.orcasound.net/graphql";

export const useAudioImages = ({
  feedId,
  startTime,
  endTime,
  enabled = true,
}: {
  feedId: string | undefined;
  startTime: Date;
  endTime: Date;
  enabled?: boolean;
}) => {
  return useQuery<AudioImagesQuery>({
    queryKey: ["audioImages", feedId, startTime, endTime],
    queryFn: async () => {
      if (!feedId) throw new Error("feedId is undefined");

      const variables: AudioImagesQueryVariables = {
        feedId,
        startTime,
        endTime,
      };

      return await request<AudioImagesQuery>(
        endpoint,
        AudioImagesDocument,
        variables,
      );
    },
    enabled: !!feedId && enabled,
    staleTime: 5 * 60 * 1000,
  });
};
