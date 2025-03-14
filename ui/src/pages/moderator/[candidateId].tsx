import {
  AccountCircle,
  Edit,
  ThumbDownOffAlt,
  ThumbUpOffAlt,
} from "@mui/icons-material";
import {
  Box,
  List,
  ListItemAvatar,
  ListItemButton,
  ListItemText,
  Typography,
} from "@mui/material";
import Grid from "@mui/material/Grid2";
import Head from "next/head";
import { useRouter } from "next/router";
import { useEffect, useState } from "react";

import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import { CandidateCardAIPlayer } from "@/components/Player/CandidateCardAIPlayer";
import { CandidateCardPlayer } from "@/components/Player/CandidateCardPlayer";
import { useData } from "@/context/DataContext";
import { useFeedsQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { CombinedData } from "@/types/DataTypes";

const CandidatePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { candidateId } = router.query;
  const startEnd =
    typeof candidateId === "string" ? candidateId?.split("_") : [];
  const startTime = startEnd.length > 0 ? new Date(startEnd[0]).getTime() : 0;
  const endTime =
    startEnd.length > 0 ? new Date(startEnd[startEnd.length - 1]).getTime() : 0;

  // replace this with a direct react-query?
  const {
    combined,
    isSuccess,
  }: { combined: CombinedData[] | undefined; isSuccess: boolean } = useData(); // this uses a context provider to call data once and make it available to all children -- this may not be better than just using the query hooks, kind of does the same thing

  // get hydrophone feed list
  const feedsQueryResult = useFeedsQuery();
  const feeds = feedsQueryResult.data?.feeds ?? [];

  type DetectionStats = {
    combined: CombinedData[];
    human: CombinedData[];
    ai: CombinedData[];
  };

  const [detections, setDetections] = useState<DetectionStats>({
    combined: [],
    human: [],
    ai: [],
  });

  useEffect(() => {
    const arr: CombinedData[] = [];
    combined?.forEach((d) => {
      const time = new Date(d.timestamp).getTime();
      if (time >= startTime && time <= endTime) {
        arr.push(d);
      }
    });
    setDetections({
      combined: arr,
      human: arr.filter((d) => d.newCategory !== "WHALE (AI)"),
      ai: arr.filter((d) => d.newCategory === "WHALE (AI)"),
    });
  }, [combined, startTime, endTime]);

  const userName = "UserProfile123";
  const aiName = "Orcahello AI";

  const feed = feeds.filter((f) => f.id === detections?.human[0]?.feedId)[0];
  const playlist = detections.human.length
    ? detections.human[0].playlistTimestamp
    : 0;

  const offsetPadding = 15;
  const minOffset =
    detections.human.length > 0
      ? Math.min(...detections.human.map((d) => +d.playerOffset))
      : 0;

  // ensures that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
  const firstPlaylist = detections.human.filter(
    (d) => +d.playlistTimestamp === playlist,
  );

  const maxOffset =
    firstPlaylist.length > 0
      ? Math.max(...firstPlaylist.map((d) => +d.playerOffset))
      : 0;

  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;

  return (
    <div>
      <Head>
        <title>
          Candidate Report {candidateId && `#${candidateId}`} | Orcasound
          Moderator
        </title>
        <meta
          name="description"
          content={`Details for acoustic detection candidate ${candidateId}`}
        />
      </Head>
      <Grid container spacing={4} height={"100vh"}>
        <Grid size={8}>
          <Box sx={{ display: "flex", justifyContent: "space-between" }}></Box>

          <Box sx={{ marginTop: 4 }}>
            <Box>
              <Typography variant="h6">
                {detections.combined[0]?.hydrophone}
              </Typography>
              <Typography variant="h4">
                {startEnd[0]
                  ? new Date(startEnd[0]).toLocaleString()
                  : "Invalid date"}
              </Typography>
            </Box>
          </Box>
          <Box p={2} />
          <div
            style={{
              display: "flex",
              overflow: "scroll",
              width: "100%",
              height: "min(382px, 50vh)",
              maxHeight: "450px",
              overflowY: "hidden",
            }}
          >
            {detections?.ai?.map((d) => (
              <Box
                key={d.spectrogramUri}
                component="img"
                src={d.spectrogramUri}
                sx={{
                  width: "100%",
                  flexBasis: 0,
                }}
              />
            ))}
            <Typography>
              &nbsp;{!isSuccess && "Waiting for Orcahello request..."}
            </Typography>
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
          >
            {detections.human.length ? (
              <CandidateCardPlayer
                feed={feed}
                playlist={playlist}
                startOffset={startOffset}
                endOffset={endOffset}
              />
            ) : detections.ai.length ? (
              <>
                <CandidateCardAIPlayer audioUri={detections.ai[0].audioUri} />
              </>
            ) : (
              "no player found"
            )}
          </Box>
          <Box className="main">
            <List aria-label="Detection comments and annotations">
              {detections.combined?.map((el, index) => (
                <ListItemButton
                  key={index}
                  aria-label={`Comment by ${el.newCategory !== "WHALE (AI)" ? userName : aiName} at ${new Date(el.timestamp).toLocaleTimeString()}`}
                >
                  <ListItemAvatar>
                    <AccountCircle
                      style={{ fontSize: 40, opacity: 0.9 }}
                      aria-hidden="true"
                    />
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
                    <Edit aria-label="Edit comment" />
                    <Box sx={{ padding: "0 8px" }} />
                    <ThumbUpOffAlt aria-label="Approve" />
                    <Box sx={{ padding: "0 8px" }} />
                    <ThumbDownOffAlt aria-label="Reject" />
                  </ListItemAvatar>
                </ListItemButton>
              ))}
            </List>
          </Box>
          <Box p={2} />
          <Typography>More things to go here include:</Typography>
          <ul>
            <li>
              Tags - initially populated by regex, can be edited by moderator
            </li>
            <li>Valentina noise analysis</li>
            <li>Dave T signal state</li>
            <li>Share / save an audio clip</li>
          </ul>
        </Grid>
        <Grid
          className="drawer"
          size={4}
          p={4}
          style={{ borderLeft: "1px solid rgba(0,0,0,.1)" }}
        >
          <Typography>Things to go here could include:</Typography>
          <ul>
            <li>Acartia map of detections in time range</li>
            <li>
              Marine Exchange of Puget Sound map of ship traffic in time range
            </li>
            <li>Local weather conditions in time range</li>
          </ul>
        </Grid>
      </Grid>
    </div>
  );
};

CandidatePage.getLayout = getModeratorLayout;

export default CandidatePage;
