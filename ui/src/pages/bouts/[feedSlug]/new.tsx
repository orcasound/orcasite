import Box from "@mui/material/Box";
import Typography from "@mui/material/Typography";
import Head from "next/head";
import { useParams, useSearchParams } from "next/navigation";
import { useMemo, useState } from "react";

import SpectrogramTimeline from "@/components/Bouts/SpectrogramTimeline";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import { BoutPlayer } from "@/components/Player/BoutPlayer";
import {
  AudioCategory,
  useDetectionsQuery,
  useFeedQuery,
  useListFeedStreamsQuery,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const NewBoutPage: NextPageWithLayout = () => {
  const params = useParams<{ feedSlug?: string }>();
  const feedSlug = params?.feedSlug;
  const searchParams = useSearchParams();
  const [playerTime, setPlayerTime] = useState<Date>();
  const audioCategory = searchParams.get("category") as AudioCategory;
  const feedQueryResult = useFeedQuery(
    { slug: feedSlug || "" },
    { enabled: !!feedSlug },
  );
  const feed = feedQueryResult.data?.feed;

  const now = useMemo(() => new Date(), []);

  // If feed is present, and there's no pre-set time,
  // get latest stream and last 10 minutes of segments.
  // Set time to end of last segment
  const feedStreamQueryResult = useListFeedStreamsQuery(
    {
      feedId: feed?.id,
      sort: { field: "START_TIME", order: "DESC" },
      limit: 1,
    },
    { enabled: !!feed?.id },
  );

  const detectionQueryResult = useDetectionsQuery(
    { feedId: feed?.id },
    { enabled: !!feed?.id },
  );

  if (!feedSlug || feedQueryResult.isLoading) return <LoadingSpinner mt={5} />;
  if (!feed) return <p>Feed not found</p>;

  const feedStreams = feedStreamQueryResult.data?.feedStreams?.results ?? [];
  const feedStream = feedStreams[0];

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
        <Box display="flex" flexDirection="column" gap={2}>
          <BoutPlayer onPlayerTimeUpdate={setPlayerTime} />
          <SpectrogramTimeline playerTime={playerTime} />
        </Box>
      </main>
    </div>
  );
};

NewBoutPage.getLayout = getSimpleLayout;

export default NewBoutPage;
