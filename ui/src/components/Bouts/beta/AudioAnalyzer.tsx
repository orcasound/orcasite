import {
  Add,
  AddCircleOutline,
  ArrowRight,
  Cached,
  Clear,
  KeyboardDoubleArrowLeft,
  KeyboardDoubleArrowRight,
  PlayArrow,
  RemoveCircleOutline,
  SkipNext,
  SkipPrevious,
  Update,
} from "@mui/icons-material";
import {
  Alert,
  Button,
  CircularProgress,
  Fade,
  FormControl,
  FormHelperText,
  IconButton,
  InputLabel,
  ListItemIcon,
  MenuItem,
  Select,
  useMediaQuery,
  useTheme,
} from "@mui/material";
import Box from "@mui/material/Box";
import Typography from "@mui/material/Typography";
import { addMinutes, format, max, min, subDays, subMinutes } from "date-fns";
import _ from "lodash";
import { useRouter } from "next/router";
import {
  SetStateAction,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import SpectrogramTimelineDark from "@/components/Bouts/beta/SpectrogramTimelineDark";
import { SpectrogramControls } from "@/components/Bouts/SpectrogramTimeline";
import { BoutPlayer, PlayerControls } from "@/components/Player/BoutPlayer";
import {
  AudioCategory,
  BoutQuery,
  FeedQuery,
  useCreateBoutMutation,
  useDetectionsQuery,
  useGenerateFeedSpectrogramsMutation,
  useUpdateBoutMutation,
} from "@/graphql/generated";
import { useAudioImages } from "@/hooks/beta/useAudioImages";
import { useListFeedStreams } from "@/hooks/beta/useListFeedStreams";
import { useAudioImageUpdatedSubscription } from "@/hooks/useAudioImageUpdatedSubscription";
import { roundToNearest } from "@/utils/time";

import CopyToClipboardButton from "../../CopyToClipboard";
import LoadingSpinner from "../../LoadingSpinner";
import BoutScrubBar from "../BoutScrubBar";
import CategoryIcon from "../CategoryIcon";

type BoutFormType = {
  errors: Record<string, string>;
  isSaving: boolean;
};

const audioCategories = Object.values(AudioCategory);

const topRightButtonHeight = "31px";

export default function AudioAnalyzer({
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
  // const { nowPlayingCandidate } = useNowPlaying();
  // const candidateStart = nowPlayingCandidate
  //   ? new Date(nowPlayingCandidate.startTimestamp)
  //   : now;
  // targetTime = targetTime ?? candidateStart;

  const [boutSaved, setBoutSaved] = useState(false);
  const [spectrogramProcessing, setSpectrogramProcessing] = useState(false);

  // const { currentUser } = useGetCurrentUserQuery().data ?? {};
  const currentUser = { moderator: true };
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

  // const candidateTitle = useMemo(() => {
  //   if (nowPlayingCandidate) {
  //     const time = formatTimestamp(nowPlayingCandidate.startTimestamp);
  //     const name = nowPlayingCandidate.hydrophone;
  //     return `${time} · ${name}`;
  //   }
  // }, [nowPlayingCandidate]);

  // const boutStartTitle = bout ? formatTimestamp(bout.startTime) : "";
  // const [boutName, setBoutName] = useState<string | undefined>(
  //   // bout?.name ?? feed.name,
  //   bout?.name ??
  //     `${formatTimestamp(boutStartTitle)} · ${feed.name}` ??
  //     feed.name,
  //   // bout?.name ?? candidateTitle ?? "New audio",
  // );
  const [boutStartTime, setBoutStartTime] = useState<Date | undefined>(
    bout?.startTime && new Date(bout.startTime),
  );
  const [boutEndTime, setBoutEndTime] = useState<Date | undefined>(
    (bout?.endTime && new Date(bout.endTime)) ?? undefined,
  );
  // const [currentTab, setCurrentTab] = useState(0);
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
        max([targetTime, addMinutes(bout?.endTime ?? targetTime, timeBuffer)]),
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
  // const feedStreamQueryResult = useListFeedStreamsQuery({
  //   feedId: feed.id,
  //   fromDateTime: timelineStartTime,
  //   toDateTime: timelineEndTime,
  //   dayBeforeFromDateTime: timelineStartTimeMinusADay,
  // });
  const feedStreamQueryResult = useListFeedStreams({
    feedId: feed.id,
    fromDateTime: timelineStartTime,
    toDateTime: timelineEndTime,
    dayBeforeFromDateTime: timelineStartTimeMinusADay,
  });

  const detectionQueryResult = useDetectionsQuery({
    feedId: feed.id,
    filter: {
      timestamp: {
        greaterThanOrEqual: playableLimits.min,
        lessThanOrEqual: playableLimits.max,
      },
    },
  });

  const feedStreams = useMemo(
    () => feedStreamQueryResult.data?.feedStreams?.results ?? [],
    [feedStreamQueryResult],
  );
  const feedStream = feedStreams[0];

  const detections = detectionQueryResult.data?.detections?.results ?? [];

  const updatedAudioImages = useAudioImageUpdatedSubscription(
    feed.id,
    timelineStartTime,
    timelineEndTime,
  );

  // const audioImagesQueryResult = useAudioImagesQuery({
  //   feedId: feed.id,
  //   startTime: timelineStartTime,
  //   endTime: timelineEndTime,
  // });
  const audioImagesQueryResult = useAudioImages({
    feedId: feed.id,
    startTime: timelineStartTime,
    endTime: timelineEndTime,
  });

  const initialAudioImages =
    audioImagesQueryResult.data?.audioImages?.results ?? [];
  console.log("initialAudioImages", JSON.stringify(initialAudioImages));

  const audioImages = _.uniqBy(
    [...updatedAudioImages, ...initialAudioImages].filter(
      (audioImage) => audioImage !== undefined && audioImage !== null,
    ),
    ({ id }) => id,
  );

  const [boutForm, setBoutForm] = useState<BoutFormType>({
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
        my={2.5}
      >
        {/* <Box> */}
        {/* <Typography variant="overline" sx={{ fontSize: 18 }}>
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
          </Typography> */}
        {/* <BoutName
          feedName={feed.name}
          boutName={boutName}
          setBoutName={setBoutName}
        /> */}
        {/* </Box> */}
        <Box
          display="flex"
          sx={{
            flexDirection: isDesktop ? "row" : "column",
            gap: "1rem",
            // marginTop: "auto",
            marginLeft: "auto",
            maxHeight: topRightButtonHeight, // matches header
          }}
        >
          <Fade in={boutSaved}>
            <Alert severity="success" sx={{ mr: 2 }}>
              Bout saved
            </Alert>
          </Fade>
          <IconButton
            title="Zoom out"
            onClick={spectrogramControls.current?.zoomOut}
          >
            <RemoveCircleOutline />
          </IconButton>
          <IconButton
            title="Zoom in"
            onClick={spectrogramControls.current?.zoomIn}
          >
            <AddCircleOutline />
          </IconButton>
          <CopyToClipboardButton text={shareUrl(cachedPlayerTime)} />

          {currentUser?.moderator && (
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
                <Cached />
              )}
            </IconButton>
          )}
          {currentUser?.moderator && (
            <CategorySelect
              boutForm={boutForm}
              audioCategory={audioCategory}
              setAudioCategory={setAudioCategory}
            />
          )}
          {!currentUser?.moderator && bout?.category && (
            <CategoryDisplay bout={bout} />
          )}
          {currentUser?.moderator && (
            <Button
              variant="contained"
              // size={isDesktop ? "large" : "small"}
              size="small"
              onClick={saveBout}
              disabled={boutForm.isSaving}
              startIcon={
                boutForm.isSaving ? (
                  <LoadingSpinner />
                ) : isNew ? (
                  <Add />
                ) : (
                  <Update />
                )
              }
              sx={{
                whiteSpace: "nowrap",
                backgroundColor: "accent4.main",
                color: "primary.main",
              }}
              // {...(boutForm.isSaving ? { startIcon: <LoadingSpinner /> } : {})}
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
      <Box display="flex" flexDirection="column" gap={2} mt={1}>
        <SpectrogramTimelineDark
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
        />{" "}
        <Box
          className="player-controls"
          display="flex"
          justifyContent="space-between"
        >
          <Box>
            <Button
              startIcon={<KeyboardDoubleArrowLeft />}
              variant="outlined"
              onClick={() => expandTimelineStart()}
              // color="secondary"
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
              // color="secondary"
              title="Expand end time"
              size="small"
              sx={{ ml: 1 }}
            >
              +{nearestMinutes} min
            </Button>
          </Box>
        </Box>
        <Box
          display="flex"
          sx={{
            gap: 2,
            justifyContent: "center",
          }}
          flexWrap="wrap"
        >
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
            {/* <Box>
              <Typography variant="overline">Bout start</Typography>
            </Box> */}
            <Box>
              <IconButton
                onClick={() =>
                  (!boutEndTime || playerTime.current < boutEndTime) &&
                  setBoutStartTime(playerTime.current)
                }
                title="Set bout start"
              >
                <SkipPrevious />
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
          <Box>
            {feedStream ? (
              <BoutPlayer
                feed={feed}
                targetTime={targetTime}
                feedStream={feedStream}
                maxTimeNum={playableLimits.max.valueOf()}
                onPlayerTimeUpdate={setPlayerTime}
                setPlayerTimeRef={setPlayerTime}
                onPlayerInit={setPlayerControls}
              />
            ) : (
              <PlayArrow className="icon" fontSize="large" />
            )}
          </Box>
          <Box
            display="flex"
            flexDirection="column"
            alignItems="center"
            minWidth={120}
          >
            {/* <Box>
              <Typography variant="overline">Bout end</Typography>
            </Box> */}
            <Box>
              <IconButton
                onClick={() =>
                  (!boutStartTime || playerTime.current > boutStartTime) &&
                  setBoutEndTime(playerTime.current)
                }
                title="Set bout end"
              >
                <SkipNext />
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
          {/* <Box
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
          </Box> */}
          {/* {currentUser?.moderator && (
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
          )} */}
        </Box>
        {/* <Box>
          <Tabs
            value={currentTab}
            onChange={(_event, value) => setCurrentTab(value)}
          >
            <Tab icon={<GraphicEq />} label="Detections" />
            {bout && <Tab icon={<Tag />} label="Tags" />}
            {currentUser?.moderator && bout && (
              <Tab icon={<Notifications />} label="Notifications" />
            )}
          </Tabs>
          <TabPanel value={currentTab} index={0}>
            <BoutDetectionsTable
              detections={detections}
              minDetectionsTime={playableLimits.min}
              maxDetectionsTime={playableLimits.max}
              playerControls={playerControls}
              spectrogramControls={spectrogramControls}
            />
          </TabPanel>
          {bout && (
            <TabPanel value={currentTab} index={1}>
              <BoutTags bout={bout} />
            </TabPanel>
          )}
          {bout && currentUser?.moderator && (
            <TabPanel value={currentTab} index={2}>
              <BoutNotifications bout={bout} />
            </TabPanel>
          )}
        </Box> */}
      </Box>
    </>
  );
}

// function TabPanel(props: {
//   children?: React.ReactNode;
//   index: number;
//   value: number;
// }) {
//   const { children, value, index, ...other } = props;

//   return (
//     <div
//       role="tabpanel"
//       hidden={value !== index}
//       id={`simple-tabpanel-${index}`}
//       aria-labelledby={`simple-tab-${index}`}
//       {...other}
//     >
//       {value === index && <Box sx={{ p: 3 }}>{children}</Box>}
//     </div>
//   );
// }

function shareUrl(time: Date) {
  const formattedTime = time.toISOString();

  const currentUrl = new URL(window.location.href);
  currentUrl.searchParams.set("time", formattedTime);
  return currentUrl.toString();
}

// Shows current name and if user is a moderator, allows setting bout name
// function BoutName({
//   feedName,
//   boutName,
//   setBoutName,
// }: {
//   feedName: string;
//   boutName?: string;
//   setBoutName: (value: SetStateAction<string | undefined>) => void;
// }) {
//   const theme = useTheme();

//   const { moderator } = useGetCurrentUserQuery().data?.currentUser ?? {
//     moderator: false,
//     // moderator: true,
//   };

//   return (
//     <Box sx={{ marginLeft: "4rem" }}>
//       {!moderator && (
//         <Typography
//           variant="h4"
//           my={1}
//           sx={{ lineHeight: 1, fontSize: "30px" }}
//         >
//           {boutName ?? feedName}
//         </Typography>
//       )}

//       {moderator && (
//         <Input
//           sx={{ ...theme.typography.h4 }}
//           disableUnderline={true}
//           type="text"
//           value={boutName ?? feedName}
//           onBlur={(event) =>
//             setBoutName(
//               (event.target.value ?? "").length > 0
//                 ? event.target.value
//                 : feedName,
//             )
//           }
//           onChange={(event) => setBoutName(event.target.value ?? feedName)}
//         />
//       )}
//     </Box>
//   );
// }

function CategorySelect({
  boutForm,
  audioCategory,
  setAudioCategory,
}: {
  boutForm: BoutFormType;
  audioCategory: AudioCategory | undefined;
  setAudioCategory: React.Dispatch<SetStateAction<AudioCategory | undefined>>;
}) {
  return (
    <Box display="flex" alignItems="center" ml={{ sm: 0, md: "auto" }}>
      <FormControl
        variant="outlined"
        sx={{
          width: "100%",
          minHeight: topRightButtonHeight, // matches header buttons
          "& .MuiInputBase-root": {
            minHeight: topRightButtonHeight, // controls input height
            height: topRightButtonHeight, // force to match container
          },
        }}
        {...(boutForm.errors.audioCategory ? { error: true } : {})}
      >
        {!audioCategory && (
          <InputLabel
            id="category-label"
            // sx={{
            //   textTransform: "uppercase",
            //   fontSize: 14,
            //   color: "primary.main",
            //   // "&[data-shrink=false]": { top: "-6px" },
            // }}
          >
            Category
          </InputLabel>
        )}
        <Select
          labelId="category-label"
          value={audioCategory ?? ""}
          onChange={(event) =>
            setAudioCategory(event.target.value as AudioCategory)
          }
          // label="Category"
          sx={{
            minWidth: 200,
            "& .MuiSelect-select": {
              display: "flex !important", // necessary to vertically center the icon
            },
          }}
          size="small"
        >
          {audioCategories.map((category) => (
            <MenuItem id="menu-item" key={category} value={category}>
              <ListItemIcon
                sx={{
                  "&": {
                    minWidth: "auto",
                    marginRight: 1,
                  },
                }}
              >
                <CategoryIcon audioCategory={category} size={15} />
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
  );
}

function CategoryDisplay({ bout }: { bout: BoutQuery["bout"] }) {
  return (
    <Box display="flex" ml={{ sm: 0, md: "auto" }} flexDirection="column">
      <Typography variant="overline" textAlign="center">
        Category
      </Typography>
      <Box display="flex" gap={1}>
        {bout?.category && (
          <CategoryIcon audioCategory={bout.category} size={25} />
        )}
        {bout?.category && (
          <Typography>{_.startCase(_.toLower(bout.category))}</Typography>
        )}
      </Box>
    </Box>
  );
}
