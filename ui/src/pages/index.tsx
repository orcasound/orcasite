import { Container, Stack, Typography } from "@mui/material";
import { dehydrate, QueryClient } from "@tanstack/react-query";
import Head from "next/head";

import FeedCard from "@/components/FeedCard";
import { getMapLayout } from "@/components/MapLayout";
import { useFeedsQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const HomePage: NextPageWithLayout = () => {
  const feeds = useFeedsQuery().data?.feeds;

  if (!feeds) return null;

  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
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

HomePage.getLayout = getMapLayout;

export async function getStaticProps() {
  const queryClient = new QueryClient();

  await queryClient.prefetchQuery(
    useFeedsQuery.getKey(),
    useFeedsQuery.fetcher(),
  );

  return {
    props: {
      dehydratedState: dehydrate(queryClient),
    },
  };
}

export default HomePage;
