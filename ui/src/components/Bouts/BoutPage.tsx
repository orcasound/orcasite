import {
  ArrowRight,
  Clear,
  Close,
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
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Fade,
  FormControl,
  FormHelperText,
  IconButton,
  InputLabel,
  Link,
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
  TextareaAutosize,
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
  Bout,
  BoutQuery,
  Candidate,
  Detection,
  FeedQuery,
  Maybe,
  useAudioImagesQuery,
  useCancelNotificationMutation,
  useCreateBoutMutation,
  useDetectionsQuery,
  useGenerateFeedSpectrogramsMutation,
  useGetCurrentUserQuery,
  useListFeedStreamsQuery,
  useNotificationsForBoutQuery,
  useNotifyLiveBoutMutation,
  useUpdateBoutMutation,
} from "@/graphql/generated";
import { useAudioImageUpdatedSubscription } from "@/hooks/useAudioImageUpdatedSubscription";
import { useBoutNotificationSentSubscription } from "@/hooks/useBoutNotificationSentSubscription";
import { formatTimestamp, roundToNearest } from "@/utils/time";

import CircularProgressWithLabel from "../CircularProgressWithLabel";
import CopyToClipboardButton from "../CopyToClipboard";
import LoadingSpinner from "../LoadingSpinner";
import BoutScrubBar from "./BoutScrubBar";
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
  const [now] = useState(() => new Date());
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
  const playerControls = useRef<PlayerControls>();
  const setPlayerControls = useCallback(
    (controls: PlayerControls) => (playerControls.current = controls),
    [],
  );
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

  // With the current implementation of the spectrogram, the play head is
  // fixed at the center of the spectrogram. The timeline start and end are about the full
  // window length, but the playable limit is at the play head's time when the spectrogram
  // is scrolled to the ends.
  const [playableLimits, setPlayableLimits] = useState<{
    min: Date;
    max: Date;
  }>({ min: timelineStartTime, max: timelineEndTime });

  const expandTimelineStart = useCallback(() => {
    setTimelineStartTime((timelineStartTime) =>
      roundToNearest(
        subMinutes(timelineStartTime, timeBuffer),
        nearestMinutes * 60 * 1000,
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
            nearestMinutes * 60 * 1000,
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
          isSaving: false,
          errors: {
            ...form.errors,
            ...Object.fromEntries(
              errors.map(({ code, message }) => [code, message] as const),
            ),
          },
        }));
      } else if (result) {
        setBoutForm((form) => ({ ...form, isSaving: false }));
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
          isSaving: false,
          errors: {
            ...form.errors,
            ...Object.fromEntries(
              errors.map(({ code, message }) => [code, message] as const),
            ),
          },
        }));
      } else {
        setBoutForm((form) => ({ ...form, isSaving: false }));
        setBoutSaved(true);
        setTimeout(() => {
          // Remove 'bout saved' message after 5 seconds
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
            <Link
              sx={{
                color: "black",
                textDecoration: "none",
                display: "flex",
                alignItems: "center",
                "&:hover": { color: (theme) => theme.palette.accent2.main },
              }}
              href={"/bouts"}
            >
              <KeyboardDoubleArrowLeft />
              Bouts
            </Link>
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
            {format(playableLimits.min, "h:mm:ss a")}
          </Box>

          <Box flexGrow={1} sx={{ mx: 2 }}>
            {feedStream?.startTime && (
              <BoutScrubBar
                feedStreamStartTimeNum={feedStream.startTime.valueOf()}
                detections={detections}
                playerTimeRef={playerTime}
                playerControls={playerControls}
                minTimeNum={playableLimits.min.valueOf()}
                maxTimeNum={playableLimits.max.valueOf()}
                spectrogramControls={spectrogramControls}
              />
            )}
          </Box>
          <Box>
            {format(playableLimits.max, "h:mm:ss a")}
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
          boutStartTime={boutStartTime}
          boutEndTime={boutEndTime}
          spectrogramControls={spectrogramControls}
          audioImages={audioImages}
          setBoutStartTime={setBoutStartTime}
          setBoutEndTime={setBoutEndTime}
          setPlayableLimits={setPlayableLimits}
        />

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
                maxTimeNum={playableLimits.max.valueOf()}
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
            {currentUser?.moderator && bout && (
              <Tab icon={<Notifications />} label="Notifications" />
            )}
          </Tabs>
          <TabPanel value={currentTab} index={0}>
            <BoutDetectionsTable
              detections={detections}
              minDetectionsTime={minDetectionsTime}
              maxDetectionsTime={maxDetectionsTime}
            />
          </TabPanel>
          {bout && currentUser?.moderator && (
            <TabPanel value={currentTab} index={1}>
              <BoutNotifications bout={bout} />
            </TabPanel>
          )}
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

function BoutDetectionsTable({
  detections,
  minDetectionsTime,
  maxDetectionsTime,
}: {
  detections: Array<
    Pick<Detection, "id" | "category" | "timestamp" | "description"> & {
      candidate?: Maybe<Pick<Candidate, "id">>;
    }
  >;
  minDetectionsTime: Date;
  maxDetectionsTime: Date;
}) {
  return (
    <Box sx={{ overflowX: "auto" }}>
      <Table>
        <TableHead>
          <TableRow>
            <TableCell>#</TableCell>
            <TableCell>ID</TableCell>
            <TableCell>Category</TableCell>
            <TableCell>Description</TableCell>
            <TableCell align="right">Timestamp</TableCell>
            <TableCell align="right">Candidate</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {detections
            .sort(({ timestamp: a }, { timestamp: b }) => {
              const date_a = new Date(a);
              const date_b = new Date(b);
              // Sort by timestamp, low to high
              return +date_a - +date_b;
            })
            .map((det, index) => (
              <TableRow key={index}>
                <TableCell>{index + 1}</TableCell>
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
  );
}

function BoutNotifications({ bout }: { bout: Pick<Bout, "id"> }) {
  const notificationsQuery = useNotificationsForBoutQuery({
    boutId: bout.id,
  });
  const initialNotifications =
    notificationsQuery.data?.notificationsForBout ?? [];

  const updatedNotifications = useBoutNotificationSentSubscription(bout.id);
  const notifications = _.uniqBy(
    [...updatedNotifications, ...initialNotifications],
    ({ id }) => id,
  ).toSorted(
    ({ insertedAt: a }, { insertedAt: b }) =>
      new Date(a).valueOf() - new Date(b).valueOf(),
  );
  const cancelNotification = useCancelNotificationMutation({
    onSuccess: () => {
      notificationsQuery.refetch();
    },
  });
  return (
    <>
      <Box sx={{ marginTop: 1 }}>
        <Box display="flex" justifyContent="space-between" alignItems="center">
          <h3>Notifications</h3>
          <Box>
            <NotificationModal
              boutId={bout.id}
              onNotification={() => notificationsQuery.refetch()}
            />
          </Box>
        </Box>
        {!notifications && <Typography>No notifications</Typography>}
        {notifications && (
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>Event</TableCell>
                <TableCell>Status</TableCell>
                <TableCell align="center">Progress</TableCell>
                <TableCell align="right">Last updated</TableCell>
                <TableCell align="right">Created</TableCell>
                <TableCell align="right">Actions</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {notifications.map((notification, index) => (
                <TableRow key={index}>
                  <TableCell>{notification.eventType?.toLowerCase()}</TableCell>
                  <TableCell>
                    <Chip
                      label={notification.active ? "Active" : "Inactive"}
                      variant="outlined"
                    />
                  </TableCell>
                  <TableCell>
                    <Box
                      display="flex"
                      alignItems="center"
                      justifyContent="center"
                    >
                      <Box sx={{ mr: 3 }}>
                        {notification.notifiedCount} /{" "}
                        {notification.targetCount}
                      </Box>
                      {typeof notification.progress === "number" && (
                        <CircularProgressWithLabel
                          value={notification.progress * 100}
                        />
                      )}
                    </Box>
                  </TableCell>
                  <TableCell
                    align="right"
                    title={notification.notifiedCountUpdatedAt?.toString()}
                  >
                    {notification.notifiedCountUpdatedAt &&
                      formatTimestamp(notification.notifiedCountUpdatedAt)}
                  </TableCell>
                  <TableCell
                    align="right"
                    title={notification.insertedAt.toString()}
                  >
                    {formatTimestamp(notification.insertedAt)}
                  </TableCell>
                  <TableCell align="right">
                    {notification.active && !notification.finished && (
                      <Button
                        onClick={() => {
                          cancelNotification.mutate({ id: notification.id });
                        }}
                      >
                        Cancel
                      </Button>
                    )}
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        )}
      </Box>
    </>
  );
}

function NotificationModal({
  boutId,
  onNotification,
}: {
  boutId: string;
  onNotification: () => void;
}) {
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState("");
  const [confirming, setConfirming] = useState(false);

  const handleOpen = () => {
    setOpen(true);
  };

  const handleClose = () => {
    setOpen(false);
    setMessage("");
    setConfirming(false);
  };

  const handleChange = (e: React.ChangeEvent<HTMLTextAreaElement>) =>
    setMessage(e.target.value);

  const handleSubmit = () => {
    setConfirming(true);
  };

  const handleConfirm = () => {
    notifyConfirmedCandidate.mutate({ boutId, message });
  };

  const notifyConfirmedCandidate = useNotifyLiveBoutMutation({
    onSuccess: () => {
      onNotification();
      handleClose();
    },
  });

  return (
    <>
      <Button onClick={handleOpen}>Notify subscribers</Button>
      <Dialog open={open} onClose={handleClose}>
        <DialogTitle>
          <Box
            display="flex"
            justifyContent="space-between"
            alignItems="center"
          >
            Notify subscribers
            <IconButton onClick={handleClose}>
              <Close />
            </IconButton>
          </Box>
        </DialogTitle>
        <DialogContent
          sx={{ minWidth: (theme) => theme.breakpoints.values.sm }}
        >
          <TextareaAutosize
            style={{ width: "100%", padding: "15px" }}
            autoFocus
            placeholder="Message to subscribers (e.g. SRKWs heard in ...)"
            onChange={handleChange}
            minRows={3}
          />
        </DialogContent>
        <DialogActions>
          {confirming ? (
            <Box
              display="flex"
              alignItems="center"
              sx={{ width: "100%" }}
              px={2}
            >
              <Button onClick={() => setConfirming(false)} color="primary">
                Cancel
              </Button>
              <Typography sx={{ marginLeft: "auto", marginRight: 2 }}>
                Are you sure?
              </Typography>
              <Button onClick={handleConfirm} color="error" variant="outlined">
                Send to subscribers
              </Button>
            </Box>
          ) : (
            <>
              <Button onClick={handleClose} color="primary">
                Cancel
              </Button>
              <Button
                onClick={handleSubmit}
                color="primary"
                variant="outlined"
                disabled={!message}
              >
                Submit
              </Button>
            </>
          )}
        </DialogActions>
      </Dialog>
    </>
  );
}
