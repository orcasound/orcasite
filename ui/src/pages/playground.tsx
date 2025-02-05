import Head from "next/head";
import { useMemo } from "react";

import { useFeeds2Query } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const FeedsPage: NextPageWithLayout = () => {
  const feedsQueryResult = useFeeds2Query();

  // Sort feeds by high latitude to low (to match the order on the map)
  const sortedFeeds = useMemo(
    () =>
      feedsQueryResult.data?.feeds.sort((a, b) => b.latLng.lat - a.latLng.lat),
    [feedsQueryResult.data],
  );

  if (!sortedFeeds) return null;

  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <pre>{JSON.stringify(feedsQueryResult.data, null, 2)}</pre>
    </div>
  );
};

//FeedsPage.getLayout = getMapLayout;

export default FeedsPage;
