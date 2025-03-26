import {
  ArrowRight,
  Clear,
  GraphicEq,
  KeyboardDoubleArrowLeft,
  KeyboardDoubleArrowRight,
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
  CircularProgress,
  Fade,
  FormControl,
  FormHelperText,
  IconButton,
  InputLabel,
  ListItemIcon,
  MenuItem,
  Select,
  Tab,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  Tabs,
  useMediaQuery,
  useTheme,
} from "@mui/material";
import Box from "@mui/material/Box";
import Typography from "@mui/material/Typography";
import { addMinutes, format, max, min, subDays, subMinutes } from "date-fns";
import _ from "lodash";
import { useRouter } from "next/router";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";

import SpectrogramTimeline, {
  SpectrogramControls,
} from "@/components/Bouts/SpectrogramTimeline";
import { BoutPlayer, PlayerControls } from "@/components/Player/BoutPlayer";
import {
  AudioCategory,
  BoutQuery,
  FeedQuery,
  useAudioImagesQuery,
  useCreateBoutMutation,
  useDetectionsQuery,
  useGenerateFeedSpectrogramsMutation,
  useGetCurrentUserQuery,
  useListFeedStreamsQuery,
  useUpdateBoutMutation,
} from "@/graphql/generated";
import { useAudioImageUpdatedSubscription } from "@/hooks/useAudioImageUpdatedSubscription";
import { formatTimestamp, roundToNearest } from "@/utils/time";

import CopyToClipboardButton from "../CopyToClipboard";
import LoadingSpinner from "../LoadingSpinner";
import CategoryIcon from "./CategoryIcon";

export default function BoutPage({
  isNew,
  feed,
  targetAudioCategory,
  targetTime,
  bout,
}: {
  isNew: boolean;
  feed: FeedQuery["feed"];
  targetAudioCategory?: AudioCategory;
  targetTime?: Date;
  bout?: BoutQuery["bout"];
}) {
  const theme = useTheme();
  const isDesktop = useMediaQuery(theme.breakpoints.up("sm"));
  const router = useRouter();
  const now = useMemo(() => new Date(), []);
  targetTime =
    targetTime ?? (bout?.startTime && new Date(bout.startTime)) ?? now;

  const [boutSaved, setBoutSaved] = useState(false);
  const [spectrogramProcessing, setSpectrogramProcessing] = useState(false);

  const { currentUser } = useGetCurrentUserQuery().data ?? {};
  const playerTime = useRef<Date>(targetTime);
  const setPlayerTime = useCallback(
    (time: Date) => (playerTime.current = time),
    [],
  );
  const [playerControls, setPlayerControls] = useState<PlayerControls>();
  const spectrogramControls = useRef<SpectrogramControls>();

  const [cachedPlayerTime, setCachedPlayerTime] = useState<Date>(targetTime);
  useEffect(() => {
    const interval = setInterval(() => {
      setCachedPlayerTime(playerTime.current);
    }, 1000);

    return () => {
      clearInterval(interval);
    };
  }, []);

  const [boutStartTime, setBoutStartTime] = useState<Date | undefined>(
    bout?.startTime && new Date(bout.startTime),
  );
  const [boutEndTime, setBoutEndTime] = useState<Date | undefined>(
    (bout?.endTime && new Date(bout.endTime)) ?? undefined,
  );
  const [currentTab, setCurrentTab] = useState(0);
  const audioCategories = Object.values(AudioCategory);
  const [audioCategory, setAudioCategory] = useState<AudioCategory | undefined>(
    targetAudioCategory ?? bout?.category,
  );

  const timeBuffer = 5; // minutes
  // Snap to nearest 5 minutes
  const nearestMinutes = 5;
  const [timelineStartTime, setTimelineStartTime] = useState<Date>(
    roundToNearest(
      subMinutes(targetTime, timeBuffer),
      nearestMinutes * 60 * 1000,
      "floor",
    ),
  );
  const [timelineEndTime, setTimelineEndTime] = useState<Date>(
    min([
      now,
      roundToNearest(
        max([targetTime, addMinutes(targetTime, timeBuffer)]),
        nearestMinutes * 60 * 1000,
        "ceil",
      ),
    ]),
  );

  const expandTimelineStart = useCallback(() => {
    setTimelineStartTime((timelineStartTime) =>
      roundToNearest(
        subMinutes(timelineStartTime, timeBuffer),
        nearestMinutes,
        "floor",
      ),
    );
  }, [nearestMinutes]);

  const expandTimelineEnd = useCallback(
    (currentTime: Date) => {
      setTimelineEndTime((timelineEndTime) =>
        min([
          currentTime,
          roundToNearest(
            addMinutes(timelineEndTime, timeBuffer),
            nearestMinutes,
            "ceil",
          ),
        ]),
      );
    },
    [nearestMinutes],
  );

  const timelineStartTimeMinusADay = subDays(timelineStartTime, 1);
  // If feed is present, and there's no pre-set time,
  // get latest stream and last <timeBuffer> minutes of segments.
  // Set time to end of last segment
  const feedStreamQueryResult = useListFeedStreamsQuery({
    feedId: feed.id,
    fromDateTime: timelineStartTime,
    toDateTime: timelineEndTime,
    dayBeforeFromDateTime: timelineStartTimeMinusADay,
  });

  // Get detections at current time plus or minus 1 hour
  const minDetectionsTime = subMinutes(targetTime, 60);
  const maxDetectionsTime = addMinutes(targetTime, 60);
  const detectionQueryResult = useDetectionsQuery({
    feedId: feed.id,
    filter: {
      timestamp: {
        greaterThanOrEqual: minDetectionsTime,
        lessThanOrEqual: maxDetectionsTime,
      },
    },
  });

  const feedStreams = useMemo(
    () => feedStreamQueryResult.data?.feedStreams?.results ?? [],
    [feedStreamQueryResult],
  );
  const feedStream = feedStreams[0];
  const feedSegments = useMemo(
    () => feedStreams.flatMap(({ feedSegments }) => feedSegments),
    [feedStreams],
  );

  const detections = detectionQueryResult.data?.detections?.results ?? [];

  const updatedAudioImages = useAudioImageUpdatedSubscription(
    feed.id,
    timelineStartTime,
    timelineEndTime,
  );

  const audioImagesQueryResult = useAudioImagesQuery({
    feedId: feed.id,
    startTime: timelineStartTime,
    endTime: timelineEndTime,
  });
  const initialAudioImages =
    audioImagesQueryResult.data?.audioImages?.results ?? [];
  const audioImages = _.uniqBy(
    [...updatedAudioImages, ...initialAudioImages].filter(
      (audioImage) => audioImage !== undefined && audioImage !== null,
    ),
    ({ id }) => id,
  );

  const [boutForm, setBoutForm] = useState<{
    errors: Record<string, string>;
    isSaving: boolean;
  }>({
    errors: {},
    isSaving: false,
  });
  const createBoutMutation = useCreateBoutMutation({
    onSuccess: ({ createBout: { errors, result } }) => {
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
      } else if (result) {
        router.push(`/bouts/${result.id}`);
      }
    },
  });

  const updateBoutMutation = useUpdateBoutMutation({
    onSuccess: ({ updateBout: { errors } }) => {
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
      } else {
        setBoutSaved(true);
        setTimeout(() => {
          setBoutSaved(false);
        }, 5000);
      }
    },
  });

  const saveBout = () => {
    setBoutForm((form) => ({ ...form, errors: {}, isSaving: true }));
    if (audioCategory && boutStartTime) {
      if (isNew) {
        createBoutMutation.mutate({
          feedId: feed.id,
          startTime: boutStartTime,
          endTime: boutEndTime,
          category: audioCategory,
        });
      } else if (bout) {
        updateBoutMutation.mutate({
          id: bout.id,
          startTime: boutStartTime,
          endTime: boutEndTime,
          category: audioCategory,
        });
      }
      setBoutForm((form) => ({ ...form, isSaving: false }));
    } else {
      const errors: Record<string, string> = {};
      if (!audioCategory) {
        errors["audioCategory"] = "Audio category required";
      }
      if (!boutStartTime) {
        errors["startTime"] = "Bout start time required";
      }
      setBoutForm((form) => ({ ...form, isSaving: false, errors }));
    }
  };

  const generateFeedSpectrograms = useGenerateFeedSpectrogramsMutation({
    onMutate: () => {
      setSpectrogramProcessing(true);
      setTimeout(() => {
        setSpectrogramProcessing(false);
      }, 30000);
    },
  });

  return (
    <>
      <Box
        display="flex"
        justifyContent="space-between"
        alignItems="center"
        my={2}
      >
        <Box>
          <Typography variant="overline" sx={{ fontSize: 18 }}>
            Bout
          </Typography>
          <Typography variant="h4">{feed.name}</Typography>
        </Box>
        <Box
          display="flex"
          sx={{
            flexDirection: isDesktop ? "row" : "column",
            marginTop: "auto",
            marginLeft: "auto",
          }}
        >
          <Fade in={boutSaved}>
            <Alert severity="success" sx={{ mr: 2 }}>
              Bout saved
            </Alert>
          </Fade>
          {currentUser?.moderator && (
            <Button
              variant="contained"
              size={isDesktop ? "large" : "small"}
              onClick={saveBout}
              disabled={boutForm.isSaving}
              sx={{ whiteSpace: "nowrap" }}
              {...(boutForm.isSaving ? { startIcon: <LoadingSpinner /> } : {})}
            >
              {isNew ? "Create" : "Update"} bout
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
        <Box display="flex" justifyContent="space-between">
          <Box>
            <Button
              startIcon={<KeyboardDoubleArrowLeft />}
              variant="outlined"
              onClick={() => expandTimelineStart()}
              color="secondary"
              title="Expand start time"
              size="small"
              sx={{ mr: 1 }}
            >
              -{nearestMinutes} min
            </Button>
            {format(timelineStartTime, "h:mm:ss a")}
          </Box>
          <Box>
            {format(timelineEndTime, "h:mm:ss a")}
            <Button
              endIcon={<KeyboardDoubleArrowRight />}
              variant="outlined"
              onClick={() => expandTimelineEnd(new Date())}
              color="secondary"
              title="Expand end time"
              size="small"
              sx={{ ml: 1 }}
            >
              +{nearestMinutes} min
            </Button>
          </Box>
        </Box>
        <SpectrogramTimeline
          playerTimeRef={playerTime}
          timelineStartTime={timelineStartTime}
          timelineEndTime={timelineEndTime}
          playerControls={playerControls}
          feedSegments={feedSegments}
          boutStartTime={boutStartTime}
          boutEndTime={boutEndTime}
          setBoutStartTime={setBoutStartTime}
          setBoutEndTime={setBoutEndTime}
          spectrogramControls={spectrogramControls}
          audioImages={audioImages}
        ></SpectrogramTimeline>

        <Box
          display="flex"
          sx={{
            gap: 2,
            justifyContent: "center",
          }}
          flexWrap="wrap"
        >
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
              <IconButton onClick={spectrogramControls.current?.zoomOut}>
                <ZoomOut />
              </IconButton>
              <IconButton onClick={spectrogramControls.current?.zoomIn}>
                <ZoomIn />
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
                    border: (theme) => `1px solid ${theme.palette.error.main}`,
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
                  {format(boutStartTime, "h:mm:ss")}
                </Button>
                {isNew && (
                  <IconButton
                    onClick={() => setBoutStartTime(undefined)}
                    title="Clear bout start"
                    size="small"
                  >
                    <Clear fontSize="small" />
                  </IconButton>
                )}
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
                  {format(boutEndTime, "h:mm:ss")}
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
          <Box
            display="flex"
            flexDirection="column"
            alignItems="center"
            minWidth={120}
          >
            <Box>
              <Typography variant="overline">Share bout</Typography>
            </Box>
            <Box>
              <CopyToClipboardButton text={shareUrl(cachedPlayerTime)} />
            </Box>
          </Box>
          {currentUser?.moderator && (
            <Box
              display="flex"
              flexDirection="column"
              alignItems="center"
              minWidth={120}
            >
              <Box>
                <Typography variant="overline">Create spectrograms</Typography>
              </Box>
              <Box>
                <IconButton
                  disabled={spectrogramProcessing}
                  onClick={() => {
                    generateFeedSpectrograms.mutate({
                      feedId: feed.id,
                      startTime: timelineStartTime,
                      endTime: timelineEndTime,
                    });
                  }}
                  title="Create spectrograms"
                >
                  {spectrogramProcessing ? (
                    <CircularProgress size={16} />
                  ) : (
                    <GraphicEq />
                  )}
                </IconButton>
              </Box>
            </Box>
          )}

          {currentUser?.moderator && (
            <Box display="flex" alignItems="center" ml={{ sm: 0, md: "auto" }}>
              <FormControl
                sx={{ width: "100%" }}
                {...(boutForm.errors.audioCategory ? { error: true } : {})}
              >
                <InputLabel
                  sx={{
                    textTransform: "uppercase",
                    fontSize: 14,
                    "&[data-shrink=false]": { top: "-6px" },
                  }}
                >
                  Category
                </InputLabel>
                <Select
                  value={audioCategory ?? ""}
                  onChange={(event) =>
                    setAudioCategory(event.target.value as AudioCategory)
                  }
                  label="Category"
                  sx={{ minWidth: 200 }}
                  size="small"
                >
                  {audioCategories.map((category) => (
                    <MenuItem key={category} value={category}>
                      <ListItemIcon
                        sx={{
                          "&": {
                            minWidth: "auto",
                            marginRight: 1,
                          },
                        }}
                      >
                        <CategoryIcon audioCategory={category} />
                      </ListItemIcon>
                      {_.startCase(_.toLower(category))}
                    </MenuItem>
                  ))}
                </Select>
                {boutForm.errors.audioCategory && (
                  <FormHelperText>Required</FormHelperText>
                )}
              </FormControl>
            </Box>
          )}
          {!currentUser?.moderator && bout?.category && (
            <Box
              display="flex"
              ml={{ sm: 0, md: "auto" }}
              flexDirection="column"
            >
              <Typography variant="overline" textAlign="center">
                Category
              </Typography>
              <Box display="flex" gap={1}>
                <CategoryIcon audioCategory={bout.category} size={25} />
                <Typography>{_.startCase(_.toLower(bout.category))}</Typography>
              </Box>
            </Box>
          )}
        </Box>
        <Box>
          <Tabs
            value={currentTab}
            onChange={(_event, value) => setCurrentTab(value)}
          >
            <Tab icon={<GraphicEq />} label="Detections" />
            {currentUser?.moderator && (
              <Tab icon={<Notifications />} label="Notifications" />
            )}
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
                      <TableCell align="right" title={det.timestamp.toString()}>
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
                          No detections submitted from{" "}
                          {format(minDetectionsTime, "h:mm a")} to{" "}
                          {format(maxDetectionsTime, "h:mm a")}
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
    </>
  );
}

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

function shareUrl(time: Date) {
  const formattedTime = time.toISOString();

  const currentUrl = new URL(window.location.href);
  currentUrl.searchParams.set("time", formattedTime);
  return currentUrl.toString();
}
