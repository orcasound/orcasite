import {
  Box,
  Button,
  InputLabel,
  List,
  ListItem,
  MenuItem,
  Select,
  TablePagination,
  Typography,
} from "@mui/material";
import { keepPreviousData } from "@tanstack/react-query";
import Head from "next/head";
import { useCallback, useMemo, useRef, useState } from "react";

import BoutItem from "@/components/Bouts/BoutItem";
import FeedItem from "@/components/Bouts/FeedItem";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import { useBoutsQuery, useFeedsQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const BoutsPage: NextPageWithLayout = () => {
  const [sortField, setSortField] = useState("name");
  const [pastBoutsPage, setPastBoutsPage] = useState(0);
  const [pastBoutsPerPage, setPastBoutsPerPage] = useState(50);
  const [sortStats, setSortStats] = useState<
    Record<string, Record<string, number>>
  >({});

  const feeds = useFeedsQuery({ sort: [{ field: "NAME", order: "ASC" }] }).data
    ?.feeds;

  const currentBouts =
    useBoutsQuery({
      filter: { endTime: { isNil: true } },
      sort: { field: "START_TIME", order: "DESC" },
    }).data?.bouts?.results ?? [];

  const pastBoutsQuery = useBoutsQuery(
    {
      filter: { endTime: { isNil: false } },
      sort: { field: "START_TIME", order: "DESC" },
      limit: pastBoutsPerPage,
      offset: pastBoutsPage * pastBoutsPerPage,
    },
    { placeholderData: keepPreviousData },
  );
  const pastBouts = pastBoutsQuery.data?.bouts?.results ?? [];
  const lastBoutsCount = useRef(0);
  const boutsCount = pastBoutsQuery.data?.bouts?.count;
  if (boutsCount != null) {
    lastBoutsCount.current = boutsCount;
  }
  const pastBoutsCount = lastBoutsCount.current;
  const pastBoutsReady = useRef(false);
  if (pastBoutsQuery.isSuccess || pastBoutsQuery.isError) {
    pastBoutsReady.current = true;
  }

  const pastBoutsPagination = {
    count: pastBoutsCount,
    page: pastBoutsPage,
    rowsPerPage: pastBoutsPerPage,
    onPageChange: (_e: unknown, newPage: number) => setPastBoutsPage(newPage),
    onRowsPerPageChange: (e: React.ChangeEvent<HTMLInputElement>) => {
      setPastBoutsPerPage(Number(e.target.value));
      setPastBoutsPage(0);
    },
    rowsPerPageOptions: [10, 50, 100],
  };

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
      (feeds ?? []).sort((a, b) => {
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
    [feeds, sortStats, sortField],
  );

  return (
    <div>
      <Head>
        <title>Bouts | Orcasound</title>
      </Head>

      <main>
        <Box
          display="flex"
          flexDirection="column"
          justifyContent="space-between"
          m={{ md: 2 }}
        >
          <h2>Current Bouts</h2>
          <Box>
            <List>
              {currentBouts.map((bout) => (
                <ListItem key={bout.id}>
                  <BoutItem bout={bout} />
                </ListItem>
              ))}
            </List>
          </Box>
        </Box>
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

        <Box
          display="flex"
          flexDirection="column"
          justifyContent="space-between"
          m={2}
        >
          <h2>Bouts</h2>
          <Box>
            {pastBoutsReady.current && (
              <TablePagination
                {...pastBoutsPagination}
                component="div"
                sx={{
                  borderBottom: 1,
                  borderColor: "divider",
                }}
              />
            )}
            {pastBoutsQuery.isError && (
              <Box display="flex" alignItems="center" gap={1} my={1}>
                <Typography color="error">
                  The bouts list failed to load.
                </Typography>
                <Button onClick={() => pastBoutsQuery.refetch()}>Retry</Button>
              </Box>
            )}
            <List sx={{ opacity: pastBoutsQuery.isPlaceholderData ? 0.5 : 1 }}>
              {pastBouts.map((bout) => (
                <ListItem key={bout.id}>
                  <BoutItem bout={bout} />
                </ListItem>
              ))}
            </List>
            {pastBoutsReady.current && (
              <TablePagination {...pastBoutsPagination} component="div" />
            )}
          </Box>
        </Box>
      </main>
    </div>
  );
};

BoutsPage.getLayout = getSimpleLayout;

export default BoutsPage;
