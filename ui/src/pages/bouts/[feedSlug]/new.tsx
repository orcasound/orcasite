import Box from "@mui/material/Box";
import Typography from "@mui/material/Typography";
import {
  addMinutes,
  min,
  roundToNearestMinutes,
  subDays,
  subMinutes,
} from "date-fns";
import Head from "next/head";
import { useParams, useSearchParams } from "next/navigation";
import { useCallback, useMemo, useRef, useState } from "react";

import SpectrogramTimeline from "@/components/Bouts/SpectrogramTimeline";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import { BoutPlayer, PlayerControls } from "@/components/Player/BoutPlayer";
import {
  AudioCategory,
  useDetectionsQuery,
  useFeedQuery,
  useListFeedStreamsQuery,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const NewBoutPage: NextPageWithLayout = () => {
  const targetTime = new Date("2024-12-11 19:55:44.013Z");
  const playerTime = useRef<Date>(targetTime);
  const setPlayerTime = useCallback(
    (time: Date) => (playerTime.current = time),
    [],
  );
  const [playerControls, setPlayerControls] = useState<PlayerControls>();

  const params = useParams<{ feedSlug?: string }>();
  const feedSlug = params?.feedSlug;
  const searchParams = useSearchParams();
  const audioCategory = searchParams.get("category") as AudioCategory;
  const [boutStartTime, setBoutStartTime] = useState<Date>();
  const [boutEndTime, setBoutEndTime] = useState<Date>();

  const feedQueryResult = useFeedQuery(
    { slug: feedSlug || "" },
    { enabled: !!feedSlug },
  );
  const feed = feedQueryResult.data?.feed;

  // Get feed segments for current time +/- 5 minute buffer
  const now = useMemo(() => new Date(), []);
  const timeBuffer = 5; // minutes
  const targetTimePlusBuffer = roundToNearestMinutes(
    min([now, addMinutes(targetTime, timeBuffer)]),
    { roundingMethod: "ceil" },
  );
  const targetTimeMinusBuffer = roundToNearestMinutes(
    subMinutes(targetTime, timeBuffer),
    { roundingMethod: "floor" },
  );
  const targetTimeMinusADay = subDays(targetTime, 1);
  // If feed is present, and there's no pre-set time,
  // get latest stream and last <timeBuffer> minutes of segments.
  // Set time to end of last segment
  const feedStreamQueryResult = useListFeedStreamsQuery(
    {
      feedId: feed?.id,
      fromDateTime: targetTimeMinusBuffer,
      toDateTime: targetTimePlusBuffer,
      dayBeforeFromDateTime: targetTimeMinusADay,
    },
    { enabled: !!feed?.id },
  );

  const detectionQueryResult = useDetectionsQuery(
    { feedId: feed?.id },
    { enabled: !!feed?.id },
  );

  const feedStreams = useMemo(
    () => feedStreamQueryResult.data?.feedStreams?.results ?? [],
    [feedStreamQueryResult],
  );
  const feedStream = feedStreams[0];
  const feedSegments = useMemo(
    () => feedStreams.flatMap(({ feedSegments }) => feedSegments),
    [feedStreams],
  );

  if (!feedSlug || feedQueryResult.isLoading) return <LoadingSpinner mt={5} />;
  if (!feed) return <p>Feed not found</p>;

  return (
    <div>
      <Head>
        <title>New Bout | Orcasound</title>
      </Head>

      <main>
        <Box display="flex" justifyContent="space-between" m={2}>
          <Box>
            <Typography variant="overline" sx={{ fontSize: 18 }}>
              New Bout
            </Typography>
            <Typography variant="h4">{feed.name}</Typography>
          </Box>
        </Box>
        <Box display="flex" flexDirection="column" gap={2}>
          {feedStream && (
            <BoutPlayer
              feed={feed}
              targetTime={targetTime}
              feedStream={feedStream}
              onPlayerTimeUpdate={setPlayerTime}
              setPlayerTimeRef={setPlayerTime}
              onPlayerInit={setPlayerControls}
            />
          )}
          <SpectrogramTimeline
            playerTimeRef={playerTime}
            timelineStartTime={targetTimeMinusBuffer}
            timelineEndTime={targetTimePlusBuffer}
            playerControls={playerControls}
            feedSegments={feedSegments}
            boutStartTime={boutStartTime}
            boutEndTime={boutEndTime}
            setBoutStartTime={setBoutStartTime}
            setBoutEndTime={setBoutEndTime}
          />
        </Box>
      </main>
    </div>
  );
};

NewBoutPage.getLayout = getSimpleLayout;

export default NewBoutPage;
