import { Container } from "@mui/material";
import Head from "next/head";

import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import type { NextPageWithLayout } from "@/pages/_app";

const CandidatePage: NextPageWithLayout = () => {
  // const router = useRouter();
  // const { candidateId } = router.query;
  // const startEnd =
  //   typeof candidateId === "string" ? candidateId?.split("_") : [];
  // const startTime = new Date(startEnd[0]).getTime();
  // const endTime = new Date(startEnd[startEnd.length - 1]).getTime();

  // const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));
  // const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  // // replace this with a direct react-query...
  // const {
  //   combined,
  //   isSuccess,
  // }: { combined: CombinedData[] | undefined; isSuccess: boolean } = useData(); // this uses a context provider to call data once and make it available to all children -- this may not be better than just using the query hooks, kind of does the same thing

  // const { filteredData, feeds } = useData();

  // type DetectionStats = {
  //   all: CombinedData[];
  //   human: HumanData[];
  //   ai: AIData[];
  // };

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

  // const [detections, setDetections] = useState<DetectionStats>({
  //   all: [],
  //   human: [],
  //   ai: [],
  // });

  // const userName = "UserProfile123";
  // const aiName = "Orcahello AI";

  // useEffect(() => {
  //   // select the detection array that matches the start/end times in the page URL
  //   const arr: CombinedData[] = [];
  //   filteredData.forEach((d) => {
  //     const time = new Date(d.timestampString).getTime();
  //     if (time >= startTime && time <= endTime) {
  //       arr.push(d);
  //     }
  //   });
  //   // store the array and separate human vs ai
  //   const humanArr = arr.filter((d) => d.newCategory !== "WHALE (AI)");
  //   const aiArr = arr.filter((d) => d.newCategory === "WHALE (AI)");
  //   setDetections({
  //     all: arr,
  //     human: humanArr,
  //     ai: aiArr,
  //   });
  //   const startTimestamp = humanArr.length ? humanArr[0].playlistTimestamp : 0;

  //   const offsetPadding = 15;
  //   const minOffset = Math.min(...humanArr.map((d) => +d.playerOffset));
  //   // const maxOffset = Math.max(...candidateArray.map((d) => +d.playerOffset));

  //   // ensures that the last offset is still in the same playlist -- future iteration may pull a second playlist if needed
  //   const firstPlaylist = humanArr.filter(
  //     (d) => +d.playlistTimestamp === startTimestamp,
  //   );

  //   const maxOffset = Math.max(...firstPlaylist.map((d) => +d.playerOffset));

  //   const startOffset = Math.max(0, minOffset - offsetPadding);
  //   const endOffset = maxOffset + offsetPadding;

  //   const feed = humanArr.length
  //     ? feeds.find((f) => f.id === humanArr[0].feedId)
  //     : feeds.find((f) => f.name === aiArr[0].hydrophone);

  return (
    <div>
      <Head>Report | Orcasound </Head>
      <Container
        maxWidth="xl"
        sx={{
          px: { xs: 1, sm: 2, md: 3 },
        }}
      >
        Nothing to see here
        {/* <Grid container spacing={4} height={"100vh"}>
          <Grid size={lgUp ? 8 : 12}>
            <Box
              sx={{ display: "flex", justifyContent: "space-between" }}
            ></Box>
            <Box sx={{ marginTop: 4 }}>
              <Box>
                <Typography variant="h6">
                  {detections.all[0]?.hydrophone}
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
              nothing to see here
            </Box>
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
        </Grid> */}
      </Container>
    </div>
  );
};

CandidatePage.getLayout = getLeftNavLayout;

export default CandidatePage;
