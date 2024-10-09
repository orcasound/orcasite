import { List, ListItem, Typography } from "@mui/material";
import Head from "next/head";

import { getReportsLayout } from "@/components/layouts/ReportsLayout";
import {
  useDetectionsQuery,
  useFeedsQuery,
  useGetCurrentUserQuery,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const BoutsPage: NextPageWithLayout = () => {
  const { currentUser } = useGetCurrentUserQuery().data ?? {};
  const timeRanges = [1, 5, 10, 30, 60, 2 * 60, 12 * 60, 24 * 60, 7 * 24 * 60];
  const currentTimeRange = 60; // minutes
  const feeds =
    useFeedsQuery({ sort: [{ field: "NAME", order: "ASC" }] }).data?.feeds ??
    [];

  const recentDetections = useDetectionsQuery().data?.detections ?? [];

  return (
    <div>
      <Head>
        <title>Bouts | Orcasound</title>
      </Head>

      <main>
        <h1>Current bouts</h1>
        <p>Ongoing bouts (ones without an end time)</p>

        <h2>Feeds</h2>
        <List>
          {feeds.map((feed, i) => (
            <ListItem key={i}>
              <Typography>{feed.name}</Typography>
            </ListItem>
          ))}
        </List>
      </main>
    </div>
  );
};

BoutsPage.getLayout = getReportsLayout;

export default BoutsPage;
