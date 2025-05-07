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
import { useEffect, useMemo, useState } from "react";

import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import type { NextPageWithLayout } from "@/pages/_app";
import { CombinedData } from "@/types/DataTypes";

const CandidatePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { candidateId } = router.query;
  const startEnd = useMemo(() => {
    return typeof candidateId === "string" ? candidateId?.split("_") : [];
  }, [candidateId]);
  const startTime = new Date(startEnd[0]).getTime();
  const endTime = new Date(startEnd[startEnd.length - 1]).getTime();

  const { filteredData, feeds } = useData();

  type DetectionStats = {
    all: CombinedData[];
    human: CombinedData[];
    ai: CombinedData[];
    hydrophone: string;
    startTime: string;
  };

  // REMOVING PLAYER --- don't need one in the detail page bc there is already the Playbar, but saving to re-evaluate with spectogram editor
  // const [playerProps, setPlayerProps] = useState({
  //   clipDateTime: "",
  //   clipNode: "",
  //   feed: feeds.length > 0 ? feeds[0] : null,
  //   image: feeds.length > 0 ? feeds[0].imageUrl : "",
  //   playlist: 0,
  //   startOffset: 0,
  //   endOffset: 0,
  //   audioUri: "",
  // });

  const [detections, setDetections] = useState<DetectionStats>({
    all: [],
    human: [],
    ai: [],
    hydrophone: "",
    startTime: "",
  });

  const userName = "UserProfile123";
  const aiName = "Orcahello AI";

  useEffect(() => {
    console.log("router.query: " + JSON.stringify(router.query, null, 2));
    console.log("router.query.candidateId: " + router.query.candidateId);
  }, [router]);

  useEffect(() => {
    // select the detection array that matches the start/end times in the page URL
    const arr: CombinedData[] = [];
    filteredData.forEach((d) => {
      const time = new Date(d.timestamp).getTime();
      if (time >= startTime && time <= endTime) {
        arr.push(d);
      }
    });
    // store the array and separate human vs ai
    const humanArr = arr.filter((d) => d.newCategory !== "WHALE (AI)");
    const aiArr = arr.filter((d) => d.newCategory === "WHALE (AI)");
    setDetections({
      all: arr,
      human: humanArr,
      ai: aiArr,
      hydrophone: arr[0]?.hydrophone,
      startTime: new Date(startEnd[0]).toLocaleString(),
    });
    // const startTimestamp = humanArr.length ? humanArr[0].playlistTimestamp : 0;

    // const offsetPadding = 15;
    // const minOffset = Math.min(...humanArr.map((d) => +d.playerOffset));
    // const maxOffset = Math.max(...candidateArray.map((d) => +d.playerOffset));

    // ensures that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
    // const firstPlaylist = humanArr.filter(
    //   (d) => +d.playlistTimestamp === startTimestamp,
    // );

    // const maxOffset = Math.max(...firstPlaylist.map((d) => +d.playerOffset));

    // const startOffset = Math.max(0, minOffset - offsetPadding);
    // const endOffset = maxOffset + offsetPadding;

    // const feed = humanArr.length
    //   ? feeds.find((f) => f.id === humanArr[0]?.feedId)
    //   : feeds.find((f) => f.name === aiArr[0]?.hydrophone);

    // use feed and start/end for the player if there are any human detections
    // if (humanArr.length) {
    //   setPlayerProps({
    //     clipDateTime: new Date(humanArr[0]?.timestamp).toLocaleString(),
    //     clipNode: feed ? feed.name : "",
    //     feed: feed ? feed : feeds[0],
    //     image: feed ? feed.imageUrl : "",
    //     playlist: startTimestamp,
    //     startOffset: startOffset,
    //     endOffset: endOffset,
    //     audioUri: "",
    //   });
    //   // otherwise, grab the audio clip from the first AI detection
    // } else if (aiArr.length) {
    //   setPlayerProps({
    //     clipDateTime: new Date(aiArr[0]?.timestamp).toLocaleString(),
    //     clipNode: feed ? feed.name : "",
    //     feed: null,
    //     image: feed ? feed.imageUrl : "",
    //     playlist: 0,
    //     startOffset: 0,
    //     endOffset: 0,
    //     audioUri: aiArr[0]?.audioUri,
    //   });
    // }
  }, [filteredData, feeds, startTime, endTime, startEnd]);

  return (
    <div style={{ overflow: "scroll" }}>
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
              <Typography variant="h4">{detections.startTime}</Typography>
            </Box>
            <Link href="./">
              <Close />
            </Link>
          </Box>
          <Box p={2} />
          <div
            style={{
              display: "flex",
              justifyContent: "center",
              alignItems: "center",
              overflowX: "auto",
              width: "100%",
              height: 300,
              border: detections.ai.length
                ? "none"
                : "1px solid rgba(255,255,255,.25)",
            }}
          >
            {detections.ai.length
              ? detections?.ai?.map((d) => (
                  <Box
                    key={d.spectrogramUri}
                    component="img"
                    src={d.spectrogramUri}
                    sx={{
                      width: "100%",
                      flexBasis: 0,
                    }}
                  />
                ))
              : "spectrogram to come"}
          </div>
          <Box p={2} />
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
                      new Date(el.timestamp).toLocaleTimeString()
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
                    {/* <ThumbUpOffAlt /> */}
                    <Box sx={{ padding: "0 8px" }} />
                    {/* <ThumbDownOffAlt /> */}
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

CandidatePage.getLayout = getHalfMapLayout;

export default CandidatePage;
