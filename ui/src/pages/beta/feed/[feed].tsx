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
import { useEffect, useState } from "react";

import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import type { NextPageWithLayout } from "@/pages/_app";
import { AIData, CombinedData, HumanData, Sighting } from "@/types/DataTypes";
import { sevenDays, subtractMilliseconds } from "@/utils/masterDataHelpers";

const HydrophonePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { candidateId } = router.query;

  const startTimestamp = subtractMilliseconds(
    new Date().toISOString(),
    sevenDays,
  ); // seven days ago
  const startTime = new Date(startTimestamp).getTime();
  const endTimestamp = new Date().toISOString(); // now
  const endTime = new Date(endTimestamp).getTime();

  const { setNowPlayingFeed, setNowPlayingCandidate } = useNowPlaying();
  const { filteredData, feeds } = useData();
  const feed = feeds.find((feed) => feed.slug === router.query.feed);

  type DetectionStats = {
    all: CombinedData[];
    human: HumanData[];
    ai: AIData[];
    sightings: Sighting[];
    hydrophone: string;
    // startTime: string;
  };

  const [detections, setDetections] = useState<DetectionStats>({
    all: [],
    human: [],
    ai: [],
    sightings: [],
    hydrophone: "",
    // startTime: "",
  });

  const userName = "UserProfile123";
  const aiName = "Orcahello AI";

  useEffect(() => {
    // select the detection array that matches the start/end times in the page URL
    const arr: CombinedData[] = [];
    filteredData.forEach((d) => {
      const time = new Date(d.timestampString).getTime();
      if (time >= startTime && time <= endTime) {
        arr.push(d);
      }
    });
    const sortedArr = arr.sort(
      (a, b) => Date.parse(a.timestampString) - Date.parse(b.timestampString),
    );

    // store the array and separate human vs ai
    const humanArr = sortedArr.filter((d) => d.type === "human");
    const aiArr = sortedArr.filter((d) => d.type === "ai");
    const sightingsArr = sortedArr.filter((d) => d.type === "sightings");
    setDetections({
      all: sortedArr,
      human: humanArr,
      ai: aiArr,
      sightings: sightingsArr,
      hydrophone: sortedArr[0]?.hydrophone,
      //   startTime: new Date(startEnd[0]).toLocaleString(),
    });
  }, [filteredData, feeds, endTime, startTime]);

  // load the feed into nowPlaying on page load
  useEffect(() => {
    if (feed) {
      setNowPlayingFeed(feed);
      setNowPlayingCandidate(null);
    }
  }, [feed, setNowPlayingCandidate, setNowPlayingFeed]);

  return (
    <div style={{ overflowY: "scroll" }}>
      <Head>Report {candidateId} | Orcasound </Head>
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
              <Typography variant="h6">{detections.hydrophone}</Typography>
              <Typography variant="h4">{`Detections since ${startTimestamp}`}</Typography>
            </Box>
            <Link href="./">
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
              {detections.all?.map((el, index) => (
                <ListItemButton key={index}>
                  <ListItemAvatar>
                    <AccountCircle style={{ fontSize: 40, opacity: 0.9 }} />
                  </ListItemAvatar>
                  <ListItemText
                    className="list-item-text"
                    primary={
                      (el.newCategory !== "WHALE (AI)" ? userName : aiName) +
                      " • " +
                      new Date(el.timestampString).toLocaleTimeString()
                    }
                    secondary={
                      el.newCategory !== "WHALE (AI)"
                        ? `${el.newCategory} • ${el.comments}`
                        : `Moderator: ${el.comments}`
                    }
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
