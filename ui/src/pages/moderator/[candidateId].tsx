import { AccountCircle, Edit } from "@mui/icons-material";
import {
  Box,
  Container,
  List,
  ListItemAvatar,
  ListItemButton,
  ListItemText,
  Theme,
  Typography,
  useMediaQuery,
} from "@mui/material";
import Grid from "@mui/material/Grid2";
import Head from "next/head";
import { useRouter } from "next/router";
import { useEffect, useState } from "react";

import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import { CandidateCardAIPlayer } from "@/components/Player/CandidateCardAIPlayer";
import { CandidateCardPlayer } from "@/components/Player/CandidateCardPlayer";
import { useData } from "@/context/DataContext";
import type { NextPageWithLayout } from "@/pages/_app";
import { CombinedData } from "@/types/DataTypes";

const CandidatePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { candidateId } = router.query;
  const startEnd =
    typeof candidateId === "string" ? candidateId?.split("_") : [];
  const startTime = new Date(startEnd[0]).getTime();
  const endTime = new Date(startEnd[startEnd.length - 1]).getTime();
  console.log("startTime: " + startTime + ", endTime: " + endTime);

  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  // replace this with a direct react-query...
  const {
    combined,
    isSuccess,
  }: { combined: CombinedData[] | undefined; isSuccess: boolean } = useData(); // this uses a context provider to call data once and make it available to all children -- this may not be better than just using the query hooks, kind of does the same thing

  // // get hydrophone feed list
  const { feeds } = useData();

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
        console.log("both true");
      }
      if (time >= startTime && time <= endTime) {
        arr.push(d);
      }
    });
    setDetections({
      combined: arr,
      human: arr.filter((d) => d.newCategory !== "WHALE (AI)"),
      ai: arr.filter((d) => d.newCategory === "WHALE (AI)"),
    });
    console.log("combined length is " + combined.length);
  }, [combined, startTime, endTime]);

  const userName = "UserProfile123";
  const aiName = "Orcahello AI";

  const feed = feeds.filter((f) => f.id === detections?.human[0]?.feedId)[0];
  const startTimestamp = detections.human.length
    ? detections.human[0].playlistTimestamp
    : 0;

  const offsetPadding = 15;
  const minOffset = Math.min(...detections.human.map((d) => +d.playerOffset));
  // const maxOffset = Math.max(...candidateArray.map((d) => +d.playerOffset));

  // ensures that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
  const firstPlaylist = detections.human.filter(
    (d) => +d.playlistTimestamp === startTimestamp,
  );

  const maxOffset = Math.max(...firstPlaylist.map((d) => +d.playerOffset));

  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;

  // const [breadcrumb, setBreadcrumb] = useState<string>("")
  // useEffect(() => {
  //   const breadcrumbName: string[] = [];
  //   detections.human.length && breadcrumbName.push(communityName);
  //   detections.ai.length && breadcrumbName.push(aiName);
  //   setBreadcrumb(breadcrumbName.join(" + "));
  // }, [detections])

  return (
    <div>
      <Head>Report {candidateId} | Orcasound </Head>
      <Container
        maxWidth="xl"
        sx={{
          px: { xs: 1, sm: 2, md: 3 },
        }}
      >
        <Grid container spacing={4} height={"100vh"}>
          <Grid size={lgUp ? 8 : 12}>
            <Box sx={{ display: "flex", justifyContent: "space-between" }}>
              {/* <Breadcrumbs sx={{ paddingTop: 4, width: "50%" }}>
          <Link underline="hover" color="inherit" href={"/candidates"}>
            Recordings
          </Link>
          <Typography>
            {breadcrumb}
          </Typography>
        </Breadcrumbs> */}
              {/* <Breadcrumbs sx={{ paddingTop: 4, justifyContent: "flex-end" }}>
          <Typography>&nbsp;{!isSuccess && "Waiting for Orcahello request..."}</Typography>
        </Breadcrumbs> */}
            </Box>

            <Box sx={{ marginTop: 4 }}>
              <Box>
                <Typography variant="h6">
                  {detections.combined[0]?.hydrophone}
                </Typography>
                <Typography variant="h4">
                  {new Date(startEnd[0]).toLocaleString()}
                </Typography>
              </Box>
            </Box>
            <Box p={2} />
            <div
              style={{
                display: "flex",
                overflow: "scroll",
                width: "100%",
                height: 382,
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
                  timestamp={startTimestamp}
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
              <List>
                {detections.combined?.map((el, index) => (
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
      </Container>
    </div>
  );
};

CandidatePage.getLayout = getModeratorLayout;

export default CandidatePage;
