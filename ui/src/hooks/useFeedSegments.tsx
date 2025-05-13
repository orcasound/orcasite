import { useQuery } from "@tanstack/react-query";
import { gql, request } from "graphql-request";

import { FeedSegment } from "@/graphql/generated";

const endpoint = "https://live.orcasound.net/graphiql/";

type FeedSegmentsResponse = {
  feedSegments: {
    results: FeedSegment[];
  };
};

const GET_SEGMENTS = (
  feedId: string,
  startTime: string,
  endTime: string,
) => gql`
  {
    feedSegments(
      feedId: "${feedId}",
      filter: {
        startTime: { greaterThanOrEqual: "${startTime}" },
        endTime: { lessThanOrEqual: "${endTime}" }
      },
      sort: { field: START_TIME, order: ASC }
    ) {
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

export const useFeedSegments = ({
  feedId,
  startTime,
  endTime,
  enabled = true,
}: {
  feedId: string | undefined;
  startTime: string;
  endTime: string;
  enabled?: boolean;
}) => {
  return useQuery<FeedSegmentsResponse>({
    queryKey: ["feedsegments", feedId, startTime, endTime],
    queryFn: async () => {
      if (!feedId) throw new Error("feedId is undefined");
      return await request(endpoint, GET_SEGMENTS(feedId, startTime, endTime));
    },
    enabled: !!feedId && !!startTime && !!endTime && enabled,
    staleTime: 5 * 60 * 1000, // optional: cache for 5 minutes
  });
};
