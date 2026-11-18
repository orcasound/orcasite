import { PlayCircleFilled } from "@mui/icons-material";
import { Box } from "@mui/material";
import {
  addMinutes,
  addSeconds,
  clamp,
  differenceInMilliseconds,
  min,
  subMilliseconds,
} from "date-fns";
import _ from "lodash";
import {
  Dispatch,
  MutableRefObject,
  SetStateAction,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import { AudioImagesQuery } from "@/graphql/generated";

import { PlayerControls } from "../Player/BoutPlayer";
import { AudioImagesLayer } from "./AudioImagesLayer";
import { BaseAudioWidthLayer } from "./BaseAudioWidthLayer";
import { FrequencyAxisLayer } from "./FrequencyAxisLayer";
import { TimelineMarker } from "./TimelineMarker";
import { TimelineTickerLayer } from "./TimelineTickerLayer";

export const TICKER_HEIGHT = 30;
const SPECTROGRAM_HEIGHT = 300;
const PIXEL_ZOOM_FACTOR = 50;
const MIN_ZOOM = 1;
const MAX_ZOOM = 1600;
const DEFAULT_ZOOM = 32;

export type SpectrogramControls = {
  goToTime: (time: Date) => void;
  zoomIn: () => void;
  zoomOut: () => void;
  skipForward: () => void;
  skipBackward: () => void;
};

// `originTime` is the time at the left edge of the scrolled content, which is
// half a window before the timeline starts (see `originTime` below).
function centerWindow(
  spectrogramWindow: MutableRefObject<HTMLDivElement | null>,
  targetTime: Date,
  originTime: Date,
  pixelsPerMinute: number,
  setWindowStartTime: (value: SetStateAction<Date | undefined>) => void,
  setWindowEndTime: (value: SetStateAction<Date | undefined>) => void,
  playerControls?: MutableRefObject<PlayerControls | undefined>,
) {
  if (spectrogramWindow.current) {
    const offset = timeToOffset(targetTime, originTime, pixelsPerMinute);
    spectrogramWindow.current.scrollLeft =
      offset - spectrogramWindow.current.clientWidth / 2;

    if (spectrogramWindow.current) {
      setWindowStartTime(
        offsetToTime(
          spectrogramWindow.current.scrollLeft,
          originTime,
          pixelsPerMinute,
        ),
      );
      setWindowEndTime(
        offsetToTime(
          spectrogramWindow.current.scrollLeft +
            spectrogramWindow.current.clientWidth,
          originTime,
          pixelsPerMinute,
        ),
      );
    }
    if (playerControls?.current?.setPlayerTime) {
      playerControls.current.setPlayerTime(targetTime);
    }
  }
}

export function timeToOffset(
  time: Date,
  timelineStartTime: Date,
  pixelsPerMinute: number,
) {
  const diffSeconds = differenceInMilliseconds(time, timelineStartTime) / 1000;
  const pixelsPerSecond = pixelsPerMinute / 60;
  return diffSeconds * pixelsPerSecond;
}

function offsetToTime(
  offset: number,
  timelineStartTime: Date,
  pixelsPerMinute: number,
) {
  return addMinutes(timelineStartTime, offset / pixelsPerMinute);
}

export function rangesOverlap(
  startTime1?: Date,
  endTime1?: Date,
  startTime2?: Date,
  endTime2?: Date,
) {
  if (startTime1 && endTime1 && startTime2 && endTime2) {
    return startTime1 <= endTime2 && endTime1 >= startTime2;
  }
  return false;
}

export default function SpectrogramTimeline({
  timelineStartTime,
  timelineEndTime,
  audioImages,
  playerTimeRef,
  playerControls,
  boutStartTime,
  boutEndTime,
  spectrogramControls,
  setPlayableLimits,
}: {
  timelineStartTime: Date;
  timelineEndTime: Date;
  audioImages: NonNullable<
    NonNullable<AudioImagesQuery["audioImages"]>["results"]
  >;
  playerTimeRef: MutableRefObject<Date>;
  playerControls?: MutableRefObject<PlayerControls | undefined>;
  boutStartTime?: Date;
  boutEndTime?: Date;
  setBoutStartTime: Dispatch<SetStateAction<Date | undefined>>;
  setBoutEndTime: Dispatch<SetStateAction<Date | undefined>>;
  spectrogramControls: MutableRefObject<SpectrogramControls | undefined>;
  setPlayableLimits: Dispatch<SetStateAction<{ min: Date; max: Date }>>;
}) {
  // Full spectrogram container
  const spectrogramWindow = useRef<HTMLDivElement | null>(null);
  const [isDragging, setIsDragging] = useState<boolean>(false);
  const [windowStartTime, setWindowStartTimeUnthrottled] = useState<Date>();
  const [windowEndTime, setWindowEndTimeUnthrottled] = useState<Date>();
  // The window's times decide which images are drawn. They are updated at most
  // twice a second, and the last update is always applied, so that the images
  // drawn match where a drag ended.
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const setWindowStartTime = useCallback(
    _.throttle(setWindowStartTimeUnthrottled, 500),
    [setWindowStartTimeUnthrottled],
  );
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const setWindowEndTime = useCallback(
    _.throttle(setWindowEndTimeUnthrottled, 500),
    [setWindowEndTimeUnthrottled],
  );
  const setPlayerTime = _.throttle(
    (time: Date) => playerControls?.current?.setPlayerTime(time),
    200,
    { trailing: false },
  );
  const wasPlaying = useRef<boolean>(false);

  const [zoomLevel, setZoomLevel] = useState<number>(DEFAULT_ZOOM);

  useEffect(() => {
    setZoomLevel(
      calculateInitialZoom({
        boutStartTime,
        boutEndTime,
        spectrogramWindow: spectrogramWindow.current,
      }),
    );
    // Only run on initial load
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // X position of visible window relative to browser
  const windowStartX = useRef<number>(0);
  // X position of how far it's scrolled from the beginning
  const windowScrollX = useRef<number>(0);
  const windowLockInterval = useRef<NodeJS.Timeout>(undefined);

  // Width of the visible window, kept up to date as the page is resized
  const [windowWidth, setWindowWidth] = useState<number>(0);
  useEffect(() => {
    const element = spectrogramWindow.current;
    if (!element) return;
    setWindowWidth(element.clientWidth);
    const observer = new ResizeObserver(() =>
      setWindowWidth(element.clientWidth),
    );
    observer.observe(element);
    return () => observer.disconnect();
  }, []);

  const pixelsPerMinute = PIXEL_ZOOM_FACTOR * zoomLevel;
  const timelineMs = differenceInMilliseconds(
    timelineEndTime,
    timelineStartTime,
  );
  const timelineWidth = (timelineMs * pixelsPerMinute) / (60 * 1000);

  // The play head is fixed at the centre of the window. For it to reach the
  // start and the end of the timeline, the scrolled content has half a window
  // of empty space before the start and after the end. Everything is laid out
  // from `originTime`, the time at the content's left edge.
  const timelineStartTimeNum = timelineStartTime.valueOf();
  const originTime = useMemo(
    () =>
      subMilliseconds(
        new Date(timelineStartTimeNum),
        (windowWidth / 2 / pixelsPerMinute) * 60 * 1000,
      ),
    [timelineStartTimeNum, windowWidth, pixelsPerMinute],
  );
  const goToTime = useCallback(
    (time: Date) => {
      centerWindow(
        spectrogramWindow,
        time,
        originTime,
        pixelsPerMinute,
        setWindowStartTime,
        setWindowEndTime,
        playerControls,
      );
    },
    [
      spectrogramWindow,
      originTime,
      pixelsPerMinute,
      playerControls,
      setWindowStartTime,
      setWindowEndTime,
    ],
  );

  if (spectrogramControls) {
    spectrogramControls.current = {
      goToTime,
      zoomIn: () =>
        setZoomLevel((zoom) => _.clamp(zoom * 2, MIN_ZOOM, MAX_ZOOM)),
      zoomOut: () =>
        setZoomLevel((zoom) => _.clamp(zoom / 2, MIN_ZOOM, MAX_ZOOM)),
      skipForward: () => {
        const duration = windowDuration({
          spectrogramWindow: spectrogramWindow.current,
          zoomLevel,
        });
        if (duration) {
          skipTime({
            goToTime,
            duration,
            timelineStartTime,
            timelineEndTime,
            playerTimeRef,
          });
        }
      },
      skipBackward: () => {
        const duration = windowDuration({
          spectrogramWindow: spectrogramWindow.current,
          zoomLevel,
        });
        if (duration) {
          skipTime({
            goToTime,
            duration: -duration,
            timelineStartTime,
            timelineEndTime,
            playerTimeRef,
          });
        }
      },
    };
  }

  useEffect(() => {
    if (spectrogramWindow.current) {
      setWindowStartTime(
        offsetToTime(
          spectrogramWindow.current.scrollLeft,
          originTime,
          pixelsPerMinute,
        ),
      );
      setWindowEndTime(
        offsetToTime(
          spectrogramWindow.current.scrollLeft +
            spectrogramWindow.current.clientWidth,
          originTime,
          pixelsPerMinute,
        ),
      );
    }
  }, [
    spectrogramWindow,
    setWindowStartTime,
    setWindowEndTime,
    originTime,
    pixelsPerMinute,
  ]);

  // Center window on current time
  useEffect(() => {
    if (!isDragging) {
      clearInterval(windowLockInterval.current);
      windowLockInterval.current = setInterval(() => {
        if (spectrogramWindow.current) {
          centerWindow(
            spectrogramWindow,
            playerTimeRef.current,
            originTime,
            pixelsPerMinute,
            setWindowStartTime,
            setWindowEndTime,
          );
        }
      }, 100);
    }
    return () => clearInterval(windowLockInterval.current);
  }, [
    isDragging,
    playerTimeRef,
    spectrogramWindow,
    pixelsPerMinute,
    originTime,
    setWindowStartTime,
    setWindowEndTime,
  ]);

  // The play head can reach every moment of the timeline.
  const timelineEndTimeNum = timelineEndTime.valueOf();
  useEffect(() => {
    setPlayableLimits({
      min: new Date(timelineStartTimeNum),
      max: new Date(timelineEndTimeNum),
    });
  }, [timelineStartTimeNum, timelineEndTimeNum, setPlayableLimits]);

  const handleTouchStart = (e: React.TouchEvent) => {
    setIsDragging(true);
    wasPlaying.current = !playerControls?.current?.paused();
    windowStartX.current =
      e.touches[0].pageX - (spectrogramWindow.current?.offsetLeft ?? 0);
    windowScrollX.current = spectrogramWindow.current?.scrollLeft ?? 0;
    playerControls?.current?.pause();
  };

  const handleTouchMove = (e: React.TouchEvent) => {
    if (!isDragging || !spectrogramWindow.current) return;
    e.preventDefault();
    const containerCursorX =
      e.touches[0].pageX - spectrogramWindow.current.offsetLeft;
    const move = containerCursorX - windowStartX.current;
    const offset = windowScrollX.current - move;
    const windowWidth = spectrogramWindow.current.clientWidth;
    if (offset < 0) return;
    if (offset > timelineWidth) return;
    spectrogramWindow.current.scrollLeft = offset;
    const targetTime = offsetToTime(
      offset + windowWidth / 2,
      originTime,
      pixelsPerMinute,
    );
    setPlayerTime(targetTime);
    setWindowStartTime(offsetToTime(offset, originTime, pixelsPerMinute));
    setWindowEndTime(
      offsetToTime(offset + windowWidth, originTime, pixelsPerMinute),
    );
  };

  const handleTouchEnd = useCallback(() => {
    setIsDragging(false);
    if (wasPlaying.current) {
      playerControls?.current?.play();
    }
  }, [playerControls]);

  const handleMouseDown = (e: React.MouseEvent) => {
    setIsDragging(true);
    wasPlaying.current = !playerControls?.current?.paused();
    windowStartX.current =
      e.pageX - (spectrogramWindow.current?.offsetLeft ?? 0);
    windowScrollX.current = spectrogramWindow.current?.scrollLeft ?? 0;
    playerControls?.current?.pause();
  };

  const handleMouseLeave = useCallback(() => {
    setIsDragging(false);
  }, []);

  const handleMouseUp = useCallback(() => {
    setIsDragging(false);
    if (wasPlaying.current) {
      playerControls?.current?.play();
    }
  }, [playerControls]);

  const handleMouseMove = (e: React.MouseEvent) => {
    if (!isDragging || !spectrogramWindow.current) return;
    e.preventDefault();
    const containerCursorX = e.pageX - spectrogramWindow.current.offsetLeft;
    const move = containerCursorX - windowStartX.current;
    const offset = windowScrollX.current - move;
    const windowWidth = spectrogramWindow.current.clientWidth;
    if (offset < 0) return;
    if (offset > timelineWidth) return;

    spectrogramWindow.current.scrollLeft = offset;
    const targetTime = offsetToTime(
      offset + windowWidth / 2,
      originTime,
      pixelsPerMinute,
    );
    playerTimeRef.current = targetTime;

    setPlayerTime(targetTime);

    setWindowStartTime(offsetToTime(offset, originTime, pixelsPerMinute));
    setWindowEndTime(
      offsetToTime(offset + windowWidth, originTime, pixelsPerMinute),
    );
  };

  useEffect(() => {
    const container = spectrogramWindow.current;
    if (container) {
      container.addEventListener("mouseleave", handleMouseLeave);
      container.addEventListener("mouseup", handleMouseUp);
      return () => {
        container.removeEventListener("mouseleave", handleMouseLeave);
        container.removeEventListener("mouseup", handleMouseUp);
      };
    }
  }, [handleMouseUp, handleMouseLeave]);

  return (
    <>
      <Box
        ref={spectrogramWindow}
        onMouseDown={handleMouseDown}
        onMouseMove={handleMouseMove}
        onTouchStart={handleTouchStart}
        onTouchMove={handleTouchMove}
        onTouchEnd={handleTouchEnd}
        onTouchCancel={handleTouchEnd}
        sx={{
          position: "relative",
          minHeight: SPECTROGRAM_HEIGHT,
          bgcolor: "#efefef",
          width: "100%",
          display: "flex",
          overflow: "hidden",
          msOverflowStyle: "none",
          "&::-webkit-scrollbar": { display: "none", height: 0 },
          scrollbarWidth: "none",
          cursor: isDragging ? "grabbing" : "grab",
          userSelect: "none",
          border: "1px solid #ccc",
          borderRadius: 2,
          boxShadow: 1,
        }}
      >
        <Box
          sx={{
            borderLeft: "2px solid #eee",
            position: "sticky",
            left: "50%",
            width: "1px",
            zIndex: 5,
            transform: `translateY(${TICKER_HEIGHT}px)`,
          }}
        ></Box>

        {boutStartTime && (
          <TimelineMarker
            time={boutStartTime}
            pixelsPerMinute={pixelsPerMinute}
            timelineStartTimeNum={originTime.valueOf()}
            zIndex={5}
            Icon={PlayCircleFilled}
            onClick={() => {
              goToTime(boutStartTime);
            }}
          />
        )}
        {boutEndTime && (
          <TimelineMarker
            time={boutEndTime}
            pixelsPerMinute={pixelsPerMinute}
            timelineStartTimeNum={originTime.valueOf()}
            zIndex={5}
            Icon={PlayCircleFilled}
            iconProps={{ transform: "scaleX(-1)" }}
            onClick={() => {
              goToTime(boutEndTime);
            }}
          />
        )}

        <FrequencyAxisLayer
          scaling="linear"
          minFrequency={1}
          maxFrequency={15000}
          zIndex={4}
        />

        {windowStartTime && windowEndTime && (
          <TimelineTickerLayer
            timelineStartTimeNum={originTime.valueOf()}
            windowStartTimeNum={windowStartTime.valueOf()}
            windowEndTimeNum={windowEndTime.valueOf()}
            pixelsPerMinute={pixelsPerMinute}
            zIndex={4}
          />
        )}
        {/* Gives the scrolled content its full width: the timeline plus half
            a window of empty space on each side. */}
        <Box
          sx={{
            position: "absolute",
            left: 0,
            top: 0,
            height: "1px",
            width: timelineWidth + windowWidth,
            pointerEvents: "none",
          }}
        />
        <BaseAudioWidthLayer
          originTimeNum={originTime.valueOf()}
          startTimeNum={timelineStartTime.valueOf()}
          endTimeNum={timelineEndTime.valueOf()}
          pixelsPerMinute={pixelsPerMinute}
          zIndex={1}
        />
        {windowStartTime && windowEndTime && (
          <AudioImagesLayer
            audioImages={audioImages}
            timelineStartTimeNum={originTime.valueOf()}
            pixelsPerMinute={pixelsPerMinute}
            windowStartTimeNum={windowStartTime.valueOf()}
            windowEndTimeNum={windowEndTime.valueOf()}
            zIndex={3}
          />
        )}
      </Box>
    </>
  );
}

function calculateInitialZoom({
  boutStartTime,
  boutEndTime,
  spectrogramWindow,
}: {
  boutStartTime?: Date;
  boutEndTime?: Date;
  spectrogramWindow: HTMLDivElement | null;
}) {
  if (boutStartTime && boutEndTime && spectrogramWindow) {
    const boutDurationMinutes =
      differenceInMilliseconds(boutEndTime, boutStartTime) / 1000 / 60;

    const boutExtent = 0.4; // Cover about 40% of the window
    const targetWindowTime = boutDurationMinutes / boutExtent;

    const windowWidth = spectrogramWindow.clientWidth;

    // zoom_factor * zoom = pixelsPerMinute
    // targetWindowTime [minutes] * pixelsPerMinute = windowWidth
    // => zoom = windowWidth / (targetWindowTime * zoom_factor)
    const zoom = windowWidth / (targetWindowTime * PIXEL_ZOOM_FACTOR);

    // Snap to nearest factor of 2
    return _.clamp(
      Math.pow(2, Math.round(Math.log2(zoom))),
      MIN_ZOOM,
      MAX_ZOOM,
    );
  } else {
    return DEFAULT_ZOOM;
  }
}

// The time duration in seconds of the spectrogram window
function windowDuration({
  spectrogramWindow,
  zoomLevel,
}: {
  spectrogramWindow: HTMLDivElement | null;
  zoomLevel: number;
}) {
  if (spectrogramWindow) {
    const windowWidth = spectrogramWindow.clientWidth;
    const pixelsPerSecond = (PIXEL_ZOOM_FACTOR * zoomLevel) / 60;

    return windowWidth / pixelsPerSecond;
  }
}

function skipTime({
  goToTime,
  duration,
  timelineStartTime,
  timelineEndTime,
  playerTimeRef,
}: {
  goToTime: (time: Date) => void;
  duration: number;
  timelineStartTime: Date;
  timelineEndTime: Date;
  playerTimeRef: MutableRefObject<Date>;
}) {
  if (playerTimeRef.current) {
    const targetTime = addSeconds(playerTimeRef.current, duration);
    const clamped = clamp(targetTime, {
      start: timelineStartTime,
      end: min([timelineEndTime, new Date()]),
    });

    return goToTime(clamped);
  }
}
