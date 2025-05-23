import { AccountCircle, Close, Edit } from "@mui/icons-material";
import {
  Box,
  Container,
  List,
  ListItemAvatar,
  ListItemButton,
  ListItemText,
  Typography,
} from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";
import { useEffect, useMemo } from "react";

import { timeRangeSelect } from "@/components/CandidateList/CandidateListFilters";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import type { NextPageWithLayout } from "@/pages/_app";

const HydrophonePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { feedSlug } = router.query;

  const { setNowPlayingFeed, setNowPlayingCandidate } = useNowPlaying();
  const { filteredData, feeds, filters } = useData();
  const feed = feeds.find((feed) => feed.slug === feedSlug);
  const detectionsThisFeed = filteredData.filter((d) => d.feedId === feed?.id);

  const timeRangeLabel = useMemo(() => {
    return timeRangeSelect.find((el) => el.value === filters.timeRange)?.label;
  }, [filters]);

  const userName = "UserProfile123";
  const aiName = "Orcahello AI";

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
            <Link href="/beta">
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
            <List>
              {detectionsThisFeed?.map((el, index) => (
                <ListItemButton key={index}>
                  <ListItemAvatar>
                    <AccountCircle style={{ fontSize: 40, opacity: 0.9 }} />
                  </ListItemAvatar>
                  <ListItemText
                    className="list-item-text"
                    primary={
                      (el.newCategory !== "WHALE (AI)" ? userName : aiName) +
                      " • " +
                      new Date(el.timestampString).toLocaleString()
                    }
                    secondary={`${el.hydrophone} • ${el.newCategory} ${el.comments ? "• " + el.comments : ""}`}
                  />
                  <ListItemAvatar sx={{ display: "flex", opacity: "0.9" }}>
                    <Edit />
                    <Box sx={{ padding: "0 8px" }} />
                    <Box sx={{ padding: "0 8px" }} />
                  </ListItemAvatar>
                </ListItemButton>
              ))}
            </List>
          </Box>
        </Box>
      </Container>
    </div>
  );
};

HydrophonePage.getLayout = getHalfMapLayout;

export default HydrophonePage;
