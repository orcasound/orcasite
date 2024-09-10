import { NavigateNext } from "@mui/icons-material";
import { Box, Breadcrumbs, Container, Typography } from "@mui/material";
import { dehydrate, QueryClient } from "@tanstack/react-query";
import Head from "next/head";
import Image from "next/legacy/image";
import { useRouter } from "next/router";

import {
  getMapLayout,
  getMapStaticProps,
} from "@/components/layouts/MapLayout";
import Link from "@/components/Link";
import { useFeedQuery, useFeedsQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const FeedPage: NextPageWithLayout = () => {
  const router = useRouter();
  const slug = router.query.feed as string;
  const feed = useFeedQuery({ slug: slug }).data?.feed;

  if (!feed) return null;

  return (
    <div>
      <Head>
        <title>{`Orcasound - ${feed.name}`}</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Box>
            <Breadcrumbs
              separator={<NavigateNext />}
              aria-label="breadcrumb"
              sx={{ mt: 2 }}
            >
              <Link href={"/"} color="inherit">
                Home
              </Link>
              <Link href={"/listen"} color="inherit">
                Listen
              </Link>
              <Typography color="textPrimary">{feed.name}</Typography>
            </Breadcrumbs>
            <h1>{feed.name}</h1>
            <div
              style={{ position: "relative", width: "100%", height: "15em" }}
            >
              {feed.imageUrl && (
                <Image
                  src={feed.imageUrl}
                  layout="fill"
                  alt=""
                  objectFit="contain"
                  objectPosition="left"
                />
              )}
            </div>
            {feed.introHtml && (
              <div dangerouslySetInnerHTML={{ __html: feed.introHtml }} />
            )}
          </Box>
        </Container>
      </main>
    </div>
  );
};

FeedPage.getLayout = getMapLayout;

export async function getStaticPaths() {
  const queryClient = new QueryClient();

  let response;
  try {
    response = await queryClient.fetchQuery({
      queryKey: useFeedsQuery.getKey(),
      queryFn: useFeedsQuery.fetcher(),
    });
  } catch (error) {
    console.error(error);
  }

  return {
    paths:
      response?.feeds.map((feed) => ({ params: { feed: feed.slug } })) ?? [],
    fallback: "blocking",
  };
}

export async function getStaticProps({ params }: { params: { feed: string } }) {
  const queryClient = new QueryClient();
  await getMapStaticProps(queryClient);
  await queryClient.prefetchQuery({
    queryKey: useFeedQuery.getKey({ slug: params.feed }),
    queryFn: useFeedQuery.fetcher({ slug: params.feed }),
  });

  return {
    props: {
      dehydratedState: dehydrate(queryClient),
    },
    revalidate: 60,
  };
}

export default FeedPage;
