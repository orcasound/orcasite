import { Container, Stack, Typography } from "@mui/material";
import { dehydrate, QueryClient } from "@tanstack/react-query";
import { useMemo } from "react";

import FeedCard from "@/components/FeedCard";
import { useFeedsQuery } from "@/graphql/generated";

const FeedList = () => {
  const feedsQueryResult = useFeedsQuery();

  // Sort feeds by high latitude to low (to match the order on the map)
  const sortedFeeds = useMemo(
    () =>
      feedsQueryResult.data?.feeds.sort((a, b) => b.latLng.lat - a.latLng.lat),
    [feedsQueryResult.data],
  );

  if (!sortedFeeds) return null;

  return (
    <Container maxWidth="sm">
      <Typography variant="h4" mt={4}>
        Listen live
      </Typography>
      <Typography variant="body1">
        Select a location to start listening live
      </Typography>
      <Stack spacing={4} mt={4}>
        {sortedFeeds.map((feed) => (
          <FeedCard key={feed.id} feed={feed} />
        ))}
      </Stack>
    </Container>
  );
};

export async function getStaticProps() {
  const queryClient = new QueryClient();

  await queryClient.prefetchQuery({
    queryKey: useFeedsQuery.getKey(),
    queryFn: useFeedsQuery.fetcher(),
  });

  return {
    props: {
      dehydratedState: dehydrate(queryClient),
    },
  };
}

export default FeedList;
