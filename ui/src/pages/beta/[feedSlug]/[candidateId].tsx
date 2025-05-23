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

import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { Feed } from "@/graphql/generated";
import { useComputedPlaybackFields } from "@/hooks/useComputedPlaybackFields";
import { useMasterData } from "@/hooks/useMasterData";
import type { NextPageWithLayout } from "@/pages/_app";
import { AIData, CombinedData, HumanData, Sighting } from "@/types/DataTypes";

const CandidatePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { candidateId, feedSlug } = router.query;
  const startEnd = useMemo(() => {
    return typeof candidateId === "string" ? candidateId?.split("_") : [];
  }, [candidateId]);
  const startTime = new Date(startEnd[0]).getTime();
  const endTime = new Date(startEnd[startEnd.length - 1]).getTime();

  const useLiveData = true;

  const { setNowPlayingCandidate, setNowPlayingFeed } = useNowPlaying();
  const { filteredData, sortedCandidates } = useData();
  const { feeds } = useMasterData(useLiveData);

  const feed = feeds?.find((f) => f.slug === feedSlug) || ({} as Feed);

  const candidate =
    sortedCandidates.find((c) => {
      return candidateId === c.id;
    }) ?? null;

  const { durationString } = useComputedPlaybackFields(candidate);

  useEffect(() => {
    setNowPlayingCandidate(candidate);
    setNowPlayingFeed(null);
  }, [candidate, setNowPlayingCandidate, setNowPlayingFeed]);

  // useEffect(() => {
  //   if(candidate) {
  //     setNowPlayingCandidate(candidate);
  //     setNowPlayingFeed(null);
  //     const {durationString} = useComputedPlaybackFields(candidate);
  //     duration = durationString;
  //   }
  // },[candidate, setNowPlayingCandidate, setNowPlayingFeed])

  // type localFeedType = { id: string; name: string; slug: string; bucket: string; visible: boolean; image_url: string; node_name: string; intro_html: string; updated_at: string; inserted_at: string; orcahello_id: null; bucket_region: string; cloudfront_url: null; dataplicity_id: null; location_point: { crs: { type: string; properties: { name: string; }; }; type: string; coordinates: number[]; }; } | undefined;
  // type audioImageType = {
  //       id: string;
  //       bucket: string;
  //       status: string;
  //       feed_id: string;
  //       end_time: string;
  //       image_size: number;
  //       image_type: string;
  //       start_time: string;
  //       object_path: string;
  // }

  // const localFeed = localFeeds.find(f => f.slug === feedSlug);
  // const audioImagesThisFeed = (localFeed: localFeedType) => audioImages.filter(f => f.feed_id === localFeed?.id)
  // const audioImagesThisTimerange = audioImagesThisFeed(localFeed)
  //   .filter(f => new Date(f.start_time) >= startDate && new Date(f.end_time) <= endDate)
  // const getObjectPaths = (audioImages: audioImageType[]) => audioImages.map(i => i.object_path);
  // const getTimes = (audioImages: audioImageType[]) => audioImages.map(i => new Date(i.start_time));

  // Audio images

  // const updatedAudioImages = useAudioImageUpdatedSubscription(
  //   feed.id,
  //   startDate,
  //   endDate,
  // );

  // const audioImagesQueryResult = useAudioImagesQuery({
  //   feedId: feed.id,
  //   startTime: startDate,
  //   endTime: endDate,
  // });

  // const initialAudioImages =
  //   audioImagesQueryResult.data?.audioImages?.results ?? [];

  // const audioImages = _.uniqBy(
  //   [...updatedAudioImages, ...initialAudioImages].filter(
  //     (audioImage) => audioImage !== undefined && audioImage !== null,
  //   ),
  //   ({ id }) => id,
  // );

  //

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

  const userName = "UserProfile123";
  const aiName = "Orcahello AI";

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
              <Typography variant="h4">{detections.startTime}</Typography>
              <Typography variant="h6">
                {detections.hydrophone}
                {" • "}
                {durationString}
              </Typography>
            </Box>
            <Link href="/beta/candidates">
              <Close />
            </Link>
          </Box>
          <Box p={2} />
          {/* <div
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
              : 
              `${JSON.stringify(audioImagesSrc, null, 2)}`}
          </div> */}
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
            </List>
          </Box>
        </Box>
      </Container>
    </div>
  );
};

CandidatePage.getLayout = getHalfMapLayout;

export default CandidatePage;
