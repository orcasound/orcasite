import { useQuery } from "@tanstack/react-query";
import request, { gql } from "graphql-request";

import { Feed } from "@/graphql/generated";

// get data on human detections using endpoint directly (gives live data in dev)
const orcasoundEndpoint = "https://live.orcasound.net/graphiql/";

const FEEDS_QUERY = gql`
  {
    feeds {
      id
      name
      slug
      nodeName
      latLng {
        lat
        lng
      }
      imageUrl
      thumbUrl
      mapUrl
      bucket
      online
      introHtml
    }
  }
`;

// live data call for Feeds
type LiveFeedsResponse = {
  feeds: Feed[];
};
const fetchLiveFeeds = (): Promise<LiveFeedsResponse> =>
  request(orcasoundEndpoint, FEEDS_QUERY);

export function useLiveFeeds() {
  const { data, isLoading, error } = useQuery({
    queryKey: ["feeds-live"],
    queryFn: fetchLiveFeeds,
  });

  return { data, isLoading, error };
}
