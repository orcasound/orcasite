import Box from "@mui/material/Box";
import Typography from "@mui/material/Typography";
import Head from "next/head";
import { useParams, useSearchParams } from "next/navigation";

import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import {
  AudioCategory,
  DetectionFilterFeedId,
  useDetectionsQuery,
  useFeedQuery,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const NewBoutPage: NextPageWithLayout = () => {
  const params = useParams<{ feedSlug?: string }>();
  const feedSlug = params?.feedSlug;
  const searchParams = useSearchParams();
  const audioCategory = searchParams.get("category") as AudioCategory;
  const feedQueryResult = useFeedQuery(
    { slug: feedSlug || "" },
    { enabled: !!feedSlug },
  );
  const feed = feedQueryResult.data?.feed;

  const detectionQueryResult = useDetectionsQuery(
    { filter: { feedId: feed?.id as DetectionFilterFeedId } },
    { enabled: !!feed?.id },
  );

  if (!feedSlug || feedQueryResult.isLoading) return <LoadingSpinner mt={5} />;
  if (!feed) return <p>Feed not found</p>;
  return (
    <div>
      <Head>
        <title>New Bout | Orcasound</title>
      </Head>

      <main>
        <Box display="flex" justifyContent="space-between" m={2}>
          <Box>
            <Typography variant="overline" sx={{ fontSize: 18 }}>
              New Bout
            </Typography>
            <Typography variant="h4">{feed.name}</Typography>
          </Box>
        </Box>
      </main>
    </div>
  );
};

NewBoutPage.getLayout = getSimpleLayout;

export default NewBoutPage;
