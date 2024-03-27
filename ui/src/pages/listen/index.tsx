import { Container, Stack, Typography } from "@mui/material";
import { dehydrate, QueryClient } from "@tanstack/react-query";
import Head from "next/head";

import FeedCard from "@/components/FeedCard";
import { getMapLayout } from "@/components/layouts/MapLayout";
import { useFeedsQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const FeedsPage: NextPageWithLayout = () => {
  const feeds = useFeedsQuery().data?.feeds;

  if (!feeds) return null;

  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Typography variant="h4" mt={4}>
            Listen live
          </Typography>
          <Typography variant="body1">
            Select a location to start listening live
          </Typography>
          <Stack spacing={4} mt={4}>
            {feeds.map((feed) => (
              <FeedCard key={feed.id} feed={feed} />
            ))}
          </Stack>
        </Container>
      </main>
    </div>
  );
};

FeedsPage.getLayout = getMapLayout;

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

export default FeedsPage;
