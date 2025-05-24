import { Close } from "@mui/icons-material";
import { Box, Container, Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";
import { useEffect, useMemo } from "react";

import { timeRangeSelect } from "@/components/CandidateList/CandidateListFilters";
import CandidatesList from "@/components/CandidateList/CandidatesList";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import type { NextPageWithLayout } from "@/pages/_app";

const HydrophonePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { feedSlug } = router.query;

  const { setNowPlayingFeed, setNowPlayingCandidate } = useNowPlaying();
  const { feeds, filters } = useData();
  const feed = feeds.find((feed) => feed.slug === feedSlug);

  const timeRangeLabel = useMemo(() => {
    return timeRangeSelect.find((el) => el.value === filters.timeRange)?.label;
  }, [filters]);

  // load the feed into nowPlaying on page load
  useEffect(() => {
    if (feed) {
      setNowPlayingFeed(feed);
      setNowPlayingCandidate(null);
    }
  }, [feed, setNowPlayingCandidate, setNowPlayingFeed]);

  return (
    <div>
      <Head>Report {feedSlug} | Orcasound </Head>
      <Container
        maxWidth="xl"
        sx={{
          px: { xs: 1, sm: 2, md: 3 },
        }}
      >
        <Box>
          <Box
            sx={{
              marginTop: 4,
              display: "flex",
              alignItems: "center",
              justifyContent: "space-between",
            }}
          >
            <Box>
              <Typography variant="h4">{timeRangeLabel}</Typography>
              <Typography variant="h6">{feed?.name}</Typography>
            </Box>
            <Link href="#" onClick={() => router.back()}>
              <Close />
            </Link>
          </Box>
          <Box
            sx={{
              display: "flex",
              alignItems: "center",
              pl: 1,
              pb: 1,
              minWidth: 250,
            }}
          ></Box>
          <Box className="main">
            <CandidatesList feed={feed} />
          </Box>
        </Box>
      </Container>
    </div>
  );
};

HydrophonePage.getLayout = getHalfMapLayout;

export default HydrophonePage;
