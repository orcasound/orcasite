import {
  Box,
  InputLabel,
  List,
  ListItem,
  MenuItem,
  Select,
} from "@mui/material";
import Head from "next/head";
import { useCallback, useMemo, useState } from "react";

import FeedItem from "@/components/Bouts/FeedItem";
import { getReportsLayout } from "@/components/layouts/ReportsLayout";
import { useFeedsQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const BoutsPage: NextPageWithLayout = () => {
  const [sortField, setSortField] = useState("name");
  const [sortStats, setSortStats] = useState<
    Record<string, Record<string, number>>
  >({});

  const feeds =
    useFeedsQuery({ sort: [{ field: "NAME", order: "ASC" }] }).data?.feeds ??
    [];

  const handleStatUpdate = useCallback(
    (feedId: string, stat: string, value: number) => {
      setSortStats((stats) => ({
        ...stats,
        [feedId]: { ...stats[feedId], [stat]: value },
      }));
    },
    [],
  );

  const sortedFeeds = useMemo(
    () =>
      feeds.sort((a, b) => {
        const nameSort = a["name"].localeCompare(b["name"]);
        if (sortField === "name") {
          // Low to high (alphabetical) for names
          return nameSort;
        } else {
          // High to low for numbers
          const sortDiff =
            sortStats[b.id][sortField] - sortStats[a.id][sortField];
          return sortDiff === 0 ? nameSort : sortDiff;
        }
      }),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [feeds.length, sortStats, sortField],
  );

  return (
    <div>
      <Head>
        <title>Bouts | Orcasound</title>
      </Head>

      <main>
        {/* <h1>Current bouts</h1>
        <p>Ongoing bouts (ones without an end time)</p> */}

        <Box display="flex" justifyContent="space-between" m={2}>
          <h2>Feeds</h2>
          <Box>
            <InputLabel sx={{ textTransform: "uppercase", fontSize: 14 }}>
              Sort by
            </InputLabel>
            <Select
              value={sortField}
              onChange={(event) => setSortField(event.target.value)}
            >
              <MenuItem value="name">Name</MenuItem>
              <MenuItem value="listeners">Listeners</MenuItem>
              <MenuItem value="detections">Detections</MenuItem>
              <MenuItem value="whale">Whale detections</MenuItem>
              <MenuItem value="vessel">Vessel detections</MenuItem>
              <MenuItem value="other">Other detections</MenuItem>
            </Select>
          </Box>
        </Box>
        <List>
          {sortedFeeds.map((feed) => (
            <ListItem key={feed.id}>
              <FeedItem feed={feed} onStatUpdate={handleStatUpdate} />
            </ListItem>
          ))}
        </List>
      </main>
    </div>
  );
};

BoutsPage.getLayout = getReportsLayout;

export default BoutsPage;
