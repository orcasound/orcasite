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
    // select the detection array that matches the start/end times in the page URL
    const arr: CombinedData[] = [];
    filteredData.forEach((d) => {
      const time = new Date(d.timestamp).getTime();
      if (time >= startTime && time <= endTime) {
        arr.push(d);
      }
    });
    const sortedArr = arr.sort(
      (a, b) => Date.parse(a.timestampString) - Date.parse(b.timestampString),
    );

    // store the array and separate human vs ai
    const humanArr = sortedArr.filter((d) => d.newCategory !== "WHALE (AI)");
    const aiArr = sortedArr.filter((d) => d.newCategory === "WHALE (AI)");
    setDetections({
      all: sortedArr,
      human: humanArr,
      ai: aiArr,
      hydrophone: sortedArr[0]?.hydrophone,
      startTime: new Date(startEnd[0]).toLocaleString(),
    });
  }, [filteredData, feeds, startTime, endTime, startEnd]);

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

CandidatePage.getLayout = getHalfMapLayout;

export default CandidatePage;
