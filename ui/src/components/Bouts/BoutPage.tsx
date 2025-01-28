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
import Image from "next/legacy/image";
import { useCallback, useMemo, useRef, useState } from "react";

import SpectrogramTimeline, {
  SpectrogramControls,
} from "@/components/Bouts/SpectrogramTimeline";
import { BoutPlayer, PlayerControls } from "@/components/Player/BoutPlayer";
import {
  AudioCategory,
  BoutQuery,
  FeedQuery,
  useCreateBoutMutation,
  useDetectionsQuery,
  useGetCurrentUserQuery,
  useListFeedStreamsQuery,
  useUpdateBoutMutation,
} from "@/graphql/generated";
import vesselIconImage from "@/public/icons/vessel-purple.svg";
import wavesIconImage from "@/public/icons/water-waves-blue.svg";
import whaleFlukeIconImage from "@/public/icons/whale-fluke-gray.svg";
import { formatTimestamp } from "@/utils/time";

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
  const now = useMemo(() => new Date(), []);
  targetTime =
    targetTime ?? (bout?.startTime && new Date(bout.startTime)) ?? now;

  const [boutSaved, setBoutSaved] = useState(false);

  const { currentUser } = useGetCurrentUserQuery().data ?? {};
  const playerTime = useRef<Date>(targetTime);
  const setPlayerTime = useCallback(
    (time: Date) => (playerTime.current = time),
    [],
  );
  const [playerControls, setPlayerControls] = useState<PlayerControls>();
  const spectrogramControls = useRef<SpectrogramControls>();

  const [boutStartTime, setBoutStartTime] = useState<Date | undefined>(
    bout?.startTime && new Date(bout.startTime),
  );
  const [boutEndTime, setBoutEndTime] = useState<Date | undefined>(
    (bout?.endTime && new Date(bout.endTime)) ?? undefined,
  );
  const [currentTab, setCurrentTab] = useState(0);
  const audioCategories: AudioCategory[] = useMemo(
    () => ["ANTHROPHONY", "BIOPHONY", "GEOPHONY"],
    [],
  );
  const [audioCategory, setAudioCategory] = useState<AudioCategory | undefined>(
    targetAudioCategory ?? bout?.category,
  );

  const timeBuffer = 15; // minutes
  const targetTimePlusBuffer = roundToNearestMinutes(
    min([targetTime, addMinutes(targetTime, timeBuffer)]),
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
  const detections = detectionQueryResult.data?.detections?.results ?? [];
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
    setBoutForm((form) => ({ ...form, errors: {} }));
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

  return (
    <>
      <Box
        display="flex"
        justifyContent="space-between"
        alignItems="center"
        m={2}
      >
        <Box>
          <Typography variant="overline" sx={{ fontSize: 18 }}>
            Bout
          </Typography>
          <Typography variant="h4">{feed.name}</Typography>
        </Box>
        <Box ml="auto" mt="auto" display="flex">
          <Fade in={boutSaved}>
            <Alert severity="success" sx={{ mr: 2 }}>
              Bout saved
            </Alert>
          </Fade>
          {currentUser?.moderator && (
            <Button variant="contained" size="large" onClick={saveBout}>
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
                  {format(boutStartTime, "hh:mm:ss")}
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

          {currentUser?.moderator && (
            <Box display="flex" alignItems="center" ml="auto">
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
                  label="Audio category"
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
            <Box display="flex" ml="auto" flexDirection="column">
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
    </>
  );
}

function CategoryIcon({
  audioCategory,
  size,
}: {
  audioCategory: AudioCategory;
  size?: number;
}) {
  size = size ?? 15;
  if (audioCategory === "BIOPHONY")
    return (
      <Image
        src={whaleFlukeIconImage.src}
        width={size}
        height={size}
        alt="Whale fluke icon"
      />
    );
  if (audioCategory === "ANTHROPHONY")
    return (
      <Image
        src={vesselIconImage.src}
        width={size}
        height={size}
        alt="Vessel icon"
      />
    );
  if (audioCategory === "GEOPHONY")
    return (
      <Image
        src={wavesIconImage.src}
        width={size}
        height={size}
        alt="Waves icon"
      />
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
