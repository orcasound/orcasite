import { useQuery } from "@tanstack/react-query";
import request, { gql } from "graphql-request";

import { Detection } from "@/graphql/generated";

// get data on human detections using endpoint directly (gives live data in dev)
const orcasoundEndpoint = "https://live.orcasound.net/graphiql/";
const DETECTIONS_QUERY = gql`
  {
    detections(limit: 250) {
      results {
        id
        feedId
        listenerCount
        category
        description
        playerOffset
        playlistTimestamp
        timestamp
        candidate {
          id
          feedId
        }
        feed {
          name
          id
        }
      }
    }
  }
`;

// live data call for Detections
type LiveDataResponse = {
  detections: {
    results: Detection[];
  };
};
const fetchLiveDetections = (): Promise<LiveDataResponse> =>
  request(orcasoundEndpoint, DETECTIONS_QUERY);

export function useLiveDetections() {
  const { data, isLoading, error } = useQuery({
    queryKey: ["detections-live"],
    queryFn: fetchLiveDetections,
  });

  return { data, isLoading, error };
}
