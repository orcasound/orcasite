import {
  ArrowRight,
  Clear,
  GraphicEq,
  Launch,
  Notifications,
  Start,
  ZoomIn,
  ZoomOut,
} from "@mui/icons-material";
import {
  Alert,
  Button,
  Chip,
  FormControl,
  FormHelperText,
  IconButton,
  InputLabel,
  MenuItem,
  Select,
  Tab,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  Tabs,
} from "@mui/material";
import Box from "@mui/material/Box";
import Typography from "@mui/material/Typography";
import {
  addMinutes,
  format,
  min,
  roundToNearestMinutes,
  subDays,
  subMinutes,
} from "date-fns";
import _ from "lodash";
import Head from "next/head";
import { useParams, useSearchParams } from "next/navigation";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";

import SpectrogramTimeline, {
  SpectrogramControls,
} from "@/components/Bouts/SpectrogramTimeline";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import { BoutPlayer, PlayerControls } from "@/components/Player/BoutPlayer";
import {
  AudioCategory,
  useCreateBoutMutation,
  useDetectionsQuery,
  useFeedQuery,
  useGetCurrentUserQuery,
  useListFeedStreamsQuery,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { formatTimestamp } from "@/utils/time";

const NewBoutPage: NextPageWithLayout = () => {
  const { currentUser } = useGetCurrentUserQuery().data ?? {};

  const targetTime = new Date("2024-12-11 19:55:44.013Z");
  const playerTime = useRef<Date>(targetTime);
  const setPlayerTime = useCallback(
    (time: Date) => (playerTime.current = time),
    [],
  );
  const [playerControls, setPlayerControls] = useState<PlayerControls>();
  const spectrogramControls = useRef<SpectrogramControls>();

  const params = useParams<{ feedSlug?: string }>();
  const feedSlug = params?.feedSlug;
  const searchParams = useSearchParams();
  const targetAudioCategory = searchParams.get("category");
  const [boutStartTime, setBoutStartTime] = useState<Date>();
  const [boutEndTime, setBoutEndTime] = useState<Date>();
  const [currentTab, setCurrentTab] = useState(0);
  const audioCategories: AudioCategory[] = useMemo(
    () => ["ANTHROPHONY", "BIOPHONY", "GEOPHONY"],
    [],
  );
  const [audioCategory, setAudioCategory] = useState<AudioCategory>();

  useEffect(() => {
    if (
      targetAudioCategory &&
      !audioCategory &&
      audioCategories.includes(_.toUpper(targetAudioCategory) as AudioCategory)
    ) {
      setAudioCategory(targetAudioCategory.toUpperCase() as AudioCategory);
    }
  }, [audioCategory, targetAudioCategory, audioCategories]);

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

  // Get detections at current time plus or minus 1 hour
  const minDetectionsTime = subMinutes(targetTime, 60);
  const maxDetectionsTime = addMinutes(targetTime, 60);
  const detectionQueryResult = useDetectionsQuery(
    {
      feedId: feed?.id,
      filter: {
        timestamp: {
          greaterThanOrEqual: minDetectionsTime,
          lessThanOrEqual: maxDetectionsTime,
        },
      },
    },
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

  const [boutForm, setBoutForm] = useState<{
    errors: Record<string, string>;
  }>({
    errors: {},
  });
  const createBoutMutation = useCreateBoutMutation({
    onSuccess: ({ createBout: { errors } }) => {
      if (errors && errors.length > 0) {
        console.error(errors);
        setBoutForm((form) => ({
          ...form,
          errors: {
            ...form.errors,
            ...Object.fromEntries(
              errors.map(({ code, message }) => [code, message] as const),
            ),
          },
        }));
      }
    },
  });

  if (!feedSlug || feedQueryResult.isLoading) return <LoadingSpinner mt={5} />;
  if (!feed) return <p>Feed not found</p>;

  const createBout = () => {
    setBoutForm((form) => ({ ...form, errors: {} }));
    if (audioCategory && boutStartTime) {
      createBoutMutation.mutate({
        feedId: feed.id,
        startTime: boutStartTime,
        endTime: boutEndTime,
        category: audioCategory,
      });
    } else {
      const errors: Record<string, string> = {};
      if (!audioCategory) {
        errors["audioCategory"] = "Audio category required";
      }
      if (!boutStartTime) {
        errors["startTime"] = "Bout start time required";
      }
      setBoutForm((form) => ({ ...form, errors }));
    }
  };
  const detections = detectionQueryResult.data?.detections?.results ?? [];

  return (
    <div>
      <Head>
        <title>New Bout | Orcasound</title>
      </Head>

      <main>
        <Box
          display="flex"
          justifyContent="space-between"
          alignItems="center"
          m={2}
        >
          <Box>
            <Typography variant="overline" sx={{ fontSize: 18 }}>
              New Bout
            </Typography>
            <Typography variant="h4">{feed.name}</Typography>
          </Box>
          <Box ml="auto">
            {currentUser?.moderator && (
              <Button variant="contained" size="large" onClick={createBout}>
                Create bout
              </Button>
            )}
          </Box>
        </Box>
        <Box display="flex" flexDirection="column">
          {Object.entries(boutForm.errors).map(([key, msg], idx) => (
            <Alert
              key={idx}
              severity="error"
              sx={{ mb: 2 }}
              onClose={() =>
                setBoutForm((form) => ({
                  ...form,
                  errors: Object.fromEntries(
                    Object.entries(form.errors).filter(
                      ([errorKey, _msg]) => key !== errorKey,
                    ),
                  ),
                }))
              }
            >
              {msg}
            </Alert>
          ))}
        </Box>
        <Box display="flex" flexDirection="column" gap={2}>
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
            spectrogramControls={spectrogramControls}
          ></SpectrogramTimeline>

          <Box display="flex" sx={{ gap: 2 }}>
            <Box minWidth={130}>
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
            </Box>
            <Box display="flex" flexDirection="column" alignItems="center">
              <Box>
                <Typography variant="overline">Zoom</Typography>
              </Box>
              <Box>
                <IconButton onClick={spectrogramControls.current?.zoomIn}>
                  <ZoomIn />
                </IconButton>
                <IconButton onClick={spectrogramControls.current?.zoomOut}>
                  <ZoomOut />
                </IconButton>
              </Box>
            </Box>

            <Box
              display="flex"
              flexDirection="column"
              alignItems="center"
              minWidth={120}
              sx={
                boutForm.errors.startTime
                  ? {
                      border: (theme) =>
                        `1px solid ${theme.palette.error.main}`,
                      borderRadius: 1,
                    }
                  : {}
              }
            >
              <Box>
                <Typography variant="overline">Bout start</Typography>
              </Box>
              <Box>
                <IconButton
                  onClick={() =>
                    (!boutEndTime || playerTime.current < boutEndTime) &&
                    setBoutStartTime(playerTime.current)
                  }
                  title="Set bout start"
                >
                  <Start />
                </IconButton>
              </Box>
              {boutStartTime && (
                <Box>
                  <Button
                    startIcon={<ArrowRight />}
                    onClick={() =>
                      spectrogramControls.current?.goToTime(boutStartTime)
                    }
                    color="secondary"
                    title="Go to bout start"
                  >
                    {format(boutStartTime, "hh:mm:ss")}
                  </Button>
                  <IconButton
                    onClick={() => setBoutStartTime(undefined)}
                    title="Clear bout start"
                    size="small"
                  >
                    <Clear fontSize="small" />
                  </IconButton>
                </Box>
              )}
            </Box>
            <Box
              display="flex"
              flexDirection="column"
              alignItems="center"
              minWidth={120}
            >
              <Box>
                <Typography variant="overline">Bout end</Typography>
              </Box>
              <Box>
                <IconButton
                  onClick={() =>
                    (!boutStartTime || playerTime.current > boutStartTime) &&
                    setBoutEndTime(playerTime.current)
                  }
                  title="Set bout end"
                >
                  <Start sx={{ transform: "rotate(180deg)" }} />
                </IconButton>
              </Box>
              {boutEndTime && (
                <Box>
                  <Button
                    startIcon={<ArrowRight />}
                    onClick={() =>
                      spectrogramControls.current?.goToTime(boutEndTime)
                    }
                    color="secondary"
                    title="Go to bout end"
                  >
                    {format(boutEndTime, "hh:mm:ss")}
                  </Button>
                  <IconButton
                    onClick={() => setBoutEndTime(undefined)}
                    title="Clear bout end"
                    size="small"
                  >
                    <Clear fontSize="small" />
                  </IconButton>
                </Box>
              )}
            </Box>

            <Box display="flex" alignItems="center" ml="auto">
              <FormControl
                sx={{ width: "100%" }}
                {...(boutForm.errors.audioCategory ? { error: true } : {})}
              >
                <InputLabel sx={{ textTransform: "uppercase", fontSize: 14 }}>
                  Audio category
                </InputLabel>
                <Select
                  value={audioCategory ?? ""}
                  onChange={(event) =>
                    setAudioCategory(event.target.value as AudioCategory)
                  }
                  label="Audio category"
                  sx={{ minWidth: 200 }}
                  size="small"
                >
                  {audioCategories.map((category) => (
                    <MenuItem key={category} value={category}>
                      {_.startCase(_.toLower(category))}
                    </MenuItem>
                  ))}
                </Select>
                {boutForm.errors.audioCategory && (
                  <FormHelperText>Required</FormHelperText>
                )}
              </FormControl>
            </Box>
            <Box display="flex" alignItems="center"></Box>
          </Box>
          <Box>
            <Tabs
              value={currentTab}
              onChange={(_event, value) => setCurrentTab(value)}
            >
              <Tab icon={<GraphicEq />} label="Detections" />
              <Tab icon={<Notifications />} label="Notifications" />
            </Tabs>
            <TabPanel value={currentTab} index={0}>
              <Box sx={{ overflowX: "auto" }}>
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell>ID</TableCell>
                      <TableCell>Category</TableCell>
                      <TableCell>Description</TableCell>
                      <TableCell align="right">Timestamp</TableCell>
                      <TableCell align="right">Candidate</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {detections.map((det, index) => (
                      <TableRow key={index}>
                        <TableCell>
                          <Typography variant="caption">{det.id}</Typography>
                        </TableCell>
                        <TableCell>
                          <Chip label={det.category} />
                        </TableCell>
                        <TableCell>{det.description}</TableCell>
                        <TableCell
                          align="right"
                          title={det.timestamp.toString()}
                        >
                          {formatTimestamp(det.timestamp)}
                        </TableCell>
                        <TableCell align="right">
                          {det?.candidate?.id && (
                            <IconButton
                              href={`/reports/${det?.candidate?.id}`}
                              target="_blank"
                              size="small"
                              sx={{ transform: "scale(0.8)" }}
                            >
                              <Launch />
                            </IconButton>
                          )}
                        </TableCell>
                      </TableRow>
                    ))}

                    {detections.length < 1 && (
                      <TableRow>
                        <TableCell colSpan={5}>
                          <Typography textAlign="center">
                            No detections submitted for{" "}
                            {format(minDetectionsTime, "hh:mm")} to{" "}
                            {format(maxDetectionsTime, "hh:mm")}
                          </Typography>
                        </TableCell>
                      </TableRow>
                    )}
                  </TableBody>
                </Table>
              </Box>
            </TabPanel>
          </Box>
        </Box>
      </main>
    </div>
  );
};

function TabPanel(props: {
  children?: React.ReactNode;
  index: number;
  value: number;
}) {
  const { children, value, index, ...other } = props;

  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`simple-tabpanel-${index}`}
      aria-labelledby={`simple-tab-${index}`}
      {...other}
    >
      {value === index && <Box sx={{ p: 3 }}>{children}</Box>}
    </div>
  );
}

NewBoutPage.getLayout = getSimpleLayout;

export default NewBoutPage;
