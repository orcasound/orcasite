import { ArrowBackIos, Close } from "@mui/icons-material";
import {
  Box,
  Button,
  Container,
  Stack,
  Theme,
  Typography,
  useMediaQuery,
} from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";
import { useEffect, useMemo, useState } from "react";

import { DetectionsList } from "@/components/CandidateList/DetectionsList";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useLayout } from "@/context/LayoutContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { Feed } from "@/graphql/generated";
import { useComputedPlaybackFields } from "@/hooks/beta/useComputedPlaybackFields";
import type { NextPageWithLayout } from "@/pages/_app";
import { AIData, CombinedData, HumanData, Sighting } from "@/types/DataTypes";
import formatDuration from "@/utils/masterDataHelpers";
import { getPageContext } from "@/utils/pageContext";
import { formatTimestamp } from "@/utils/time";

const CandidatePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { isFeedDetail } = getPageContext(router);
  const { feeds, filters, autoPlayOnReady } = useData();
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  const { candidateId, feedSlug } = router.query;
  const startEnd = useMemo(() => {
    return typeof candidateId === "string" ? candidateId?.split("_") : [];
  }, [candidateId]);
  const startTime = new Date(startEnd[0]).getTime();
  const endTime = new Date(startEnd[startEnd.length - 1]).getTime();

  const { setNowPlayingCandidate, setNowPlayingFeed } = useNowPlaying();
  const { filteredData, sortedCandidates } = useData();
  const { playbarExpanded, setPlaybarExpanded, setMobileTab } = useLayout();

  const feed = feeds?.find((f) => f.slug === feedSlug) || ({} as Feed);

  const candidate =
    sortedCandidates.find((c) => {
      return candidateId === c.id;
    }) ?? null;

  // if the card is rendered on feed detail, show candidateFeedHref
  const feedDetailHref = `/beta/${feed?.slug}/candidates`;
  // const feedDetailCandidateHref = `/beta/${feed?.slug}/${candidate?.id}`;

  // if the card is rendered on browse all candidates, show candidateBrowseHref
  const allCandidatesHref = `/beta`;
  // const allCandidatesDetailHref = `/beta/candidates/${feed?.slug}/${candidate?.id}`;

  const closeHref = !isFeedDetail ? allCandidatesHref : feedDetailHref;

  const { durationString } = useComputedPlaybackFields(candidate);

  type DetectionStats = {
    all: CombinedData[];
    human: HumanData[];
    ai: AIData[];
    sightings: Sighting[];
    hydrophone: string;
    startTime: string;
  };

  const [detections, setDetections] = useState<DetectionStats>({
    all: [],
    human: [],
    ai: [],
    sightings: [],
    hydrophone: "",
    startTime: "",
  });

  useEffect(() => {
    // select the detection array that matches the feed and start/end times in the page URL
    const arr: CombinedData[] = [];
    filteredData.forEach((d) => {
      const time = new Date(d.timestamp.toString()).getTime();
      if (time >= startTime && time <= endTime && d.feedId === feed.id) {
        arr.push(d);
      }
    });
    const sortedArr = arr.sort(
      (a, b) =>
        Date.parse(a.timestamp.toString()) - Date.parse(b.timestamp.toString()),
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
      startTime: new Date(startEnd[0]).toLocaleString(),
    });
  }, [filteredData, feeds, startTime, endTime, startEnd, feed.id]);

  useEffect(() => {
    window.scrollTo(0, 0);
  }, []);

  useEffect(() => {
    setNowPlayingCandidate(candidate);
    setNowPlayingFeed(null);
  }, [setNowPlayingCandidate, setNowPlayingFeed, candidate]);

  const candidateStart = candidate?.startTimestamp ?? "";
  const currentTimeSeconds = new Date().getTime() / 1000;
  const timestampSeconds = new Date(candidateStart).getTime() / 1000;
  const timeAgoString = formatDuration(timestampSeconds, currentTimeSeconds);

  return (
    <div>
      <Head>Report {candidateId} | Orcasound </Head>
      <Container
        maxWidth="xl"
        sx={{
          p: 2,
          pb: 6,
        }}
      >
        <Box>
          {" "}
          {smDown && (
            <Link
              href={"#"}
              onClick={(e) => {
                e.preventDefault();
                router.back();
              }}
              style={{
                display: "flex",
                alignItems: "center",
                gap: "8px",
                textDecoration: "none",
                lineHeight: 1,
                color: "common.white",
                zIndex: 1,
                position: "relative",
              }}
            >
              <ArrowBackIos />
            </Link>
          )}
          <Box
            sx={{
              marginTop: 1,
              display: "flex",
              justifyContent: "space-between",
            }}
          >
            <Box>
              <Typography variant="h5" sx={{ lineHeight: 1, my: ".5rem" }}>
                {formatTimestamp(detections.startTime)}
              </Typography>

              <Typography
                variant="h6"
                sx={{ lineHeight: 1.2, opacity: 0.75, mb: "4px" }}
              >
                {detections.hydrophone}
              </Typography>

              <Typography
                variant="body1"
                sx={{ lineHeight: 1.2, opacity: 0.75 }}
              >
                {timeAgoString} ago
                {" · "}
                {durationString}
                {" · "}
                Reports within {filters?.timeIncrement} min
              </Typography>
            </Box>
            {!smDown && (
              <Link
                href={closeHref}
                onClick={() => {
                  setNowPlayingFeed(feed);
                  setNowPlayingCandidate(null);
                  autoPlayOnReady.current = false;
                }}
              >
                <Close />
              </Link>
            )}
          </Box>
          <Stack gap={2} direction="column" sx={{ my: 3 }}>
            {durationString !== "audio unavailable" ? (
              <Button
                variant="contained"
                onClick={() => {
                  setPlaybarExpanded(!playbarExpanded);
                }}
                sx={{
                  width: "100%",
                }}
              >
                {playbarExpanded ? "Return to map" : "Open audio analyzer"}
              </Button>
            ) : (
              <Box sx={{ my: "1rem" }}></Box>
            )}
            {smDown && (
              <Button
                variant="outlined"
                sx={{ width: "100%" }}
                onClick={() => {
                  setMobileTab(0);
                  router.push("/beta");
                }}
              >
                Open map view
              </Button>
            )}
          </Stack>
          <Box className="main">
            {candidate && <DetectionsList candidate={candidate} />}
            {/* <List>
              {candidate &&
                candidate.array?.map((el, index) => (
                  <ListItemButton key={index}>
                    <ListItemAvatar>
                      <AccountCircle style={{ fontSize: 40, opacity: 0.9 }} />
                    </ListItemAvatar>
                    <ListItemText
                      className="list-item-text"
                      primary={
                        el.hydrophone +
                        " • " +
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
            </List> */}
          </Box>
        </Box>
      </Container>
    </div>
  );
};

CandidatePage.getLayout = getHalfMapLayout;

export default CandidatePage;
