import { useQuery } from "@tanstack/react-query";
import { gql, request } from "graphql-request";

import { FeedStream } from "@/graphql/generated";

const endpoint = "https://live.orcasound.net/graphiql/";

type FeedStreamResponse = {
  feedStreams: {
    results: FeedStream[];
  };
};

const GET_STREAMS = (feedId: string, playlistTimestamp: string) => gql`
  {
  feedStreams(
    feedId: "${feedId}",
    filter: {playlistTimestamp: {eq: "${playlistTimestamp}"}}) {
    results {
      startTime
      endTime
      duration
      bucket
      bucketRegion
      cloudfrontUrl
      playlistM3u8Path
      playlistPath
      playlistTimestamp
      feedId
    }
  }
}
`;

export const useFeedStreams = ({
  feedId,
  playlistTimestamp,
  enabled = true,
}: {
  feedId: string | undefined;
  playlistTimestamp: string;
  enabled?: boolean;
}) => {
  return useQuery<FeedStreamResponse>({
    queryKey: ["feedstreams", feedId, playlistTimestamp],
    queryFn: async () => {
      if (!feedId) throw new Error("feedId is undefined");
      return await request(endpoint, GET_STREAMS(feedId, playlistTimestamp));
    },
    enabled: !!feedId && !!playlistTimestamp && enabled,
    staleTime: 5 * 60 * 1000, // optional: cache for 5 minutes
  });
};
