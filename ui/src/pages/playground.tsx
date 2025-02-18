import Head from "next/head";

import { getDashboardLayout } from "@/components/layouts/DashboardLayout";
import { useDetectionsQuery, useFeeds2Query } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const FeedsPage: NextPageWithLayout = () => {
  const feedsQueryResult = useFeeds2Query();

  const detectionQueryResult = useDetectionsQuery();

  /*
const recentDetections = detectionQueryResult.data?.detections?.results ?? [];
const timestamp = recentDetections[0].timestamp;
const reformatted = new Date(timestamp);
recentDetections[0] = reformatted.toLocaleDateString(); DONT DO THIS
*/

  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>
      <pre>{JSON.stringify(detectionQueryResult.data, null, 2)}</pre>
      <pre>{JSON.stringify(feedsQueryResult.data, null, 2)}</pre>
    </div>
  );
};

FeedsPage.getLayout = getDashboardLayout;

export default FeedsPage;
