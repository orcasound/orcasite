import { PlayCircleFilled } from "@mui/icons-material";
import { Box } from "@mui/material";
import { addMinutes, differenceInMilliseconds } from "date-fns";
import _ from "lodash";
import {
  Dispatch,
  MutableRefObject,
  SetStateAction,
  useCallback,
  useEffect,
  useRef,
  useState,
} from "react";

import { AudioImage, FeedSegment } from "@/graphql/generated";

import { PlayerControls } from "../Player/BoutPlayer";
import { BaseAudioWidthLayer } from "./BaseAudioWidthLayer";
import { FeedSegmentsLayer } from "./FeedSegmentsLayer";
import { FrequencyAxisLayer } from "./FrequencyAxisLayer";
import { TimelineMarker } from "./TimelineMarker";
import { TimelineTickerLayer } from "./TimelineTickerLayer";

export const TICKER_HEIGHT = 30;
const SPECTROGRAM_HEIGHT = 300;
const PIXEL_ZOOM_FACTOR = 50;

type SpectrogramFeedSegment = Pick<
  FeedSegment,
  "id" | "startTime" | "endTime" | "duration"
> & { audioImages: Pick<AudioImage, "bucket" | "objectPath">[] };

export type SpectrogramControls = {
  goToTime: (time: Date) => void;
  zoomIn: () => void;
  zoomOut: () => void;
};

function centerWindow(
  spectrogramWindow: MutableRefObject<HTMLDivElement | null>,
  targetTime: Date,
  timelineStartTime: Date,
  pixelsPerMinute: number,
  setWindowStartTime: (value: SetStateAction<Date | undefined>) => void,
  setWindowEndTime: (value: SetStateAction<Date | undefined>) => void,
  playerControls?: PlayerControls,
) {
  if (spectrogramWindow.current) {
    const offset = timeToOffset(targetTime, timelineStartTime, pixelsPerMinute);
    spectrogramWindow.current.scrollLeft =
      offset - spectrogramWindow.current.clientWidth / 2;

    if (spectrogramWindow.current) {
      setWindowStartTime(
        offsetToTime(
          spectrogramWindow.current.scrollLeft,
          timelineStartTime,
          pixelsPerMinute,
        ),
      );
      setWindowEndTime(
        offsetToTime(
          spectrogramWindow.current.scrollLeft +
            spectrogramWindow.current.clientWidth,
          timelineStartTime,
          pixelsPerMinute,
        ),
      );
    }
    if (playerControls?.setPlayerTime) {
      playerControls.setPlayerTime(targetTime);
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
  feedSegments,
  playerTimeRef,
  playerControls,
  boutStartTime,
  boutEndTime,
  spectrogramControls,
}: {
  timelineStartTime: Date;
  timelineEndTime: Date;
  feedSegments: SpectrogramFeedSegment[];
  playerTimeRef: MutableRefObject<Date>;
  playerControls?: PlayerControls;
  boutStartTime?: Date;
  boutEndTime?: Date;
  setBoutStartTime: Dispatch<SetStateAction<Date | undefined>>;
  setBoutEndTime: Dispatch<SetStateAction<Date | undefined>>;
  spectrogramControls: MutableRefObject<SpectrogramControls | undefined>;
}) {
  // Full spectrogram container
  const spectrogramWindow = useRef<HTMLDivElement | null>(null);
  const [isDragging, setIsDragging] = useState<boolean>(false);
  const [zoomLevel, setZoomLevel] = useState<number>(8);
  const [windowStartTime, setWindowStartTimeUnthrottled] = useState<Date>();
  const [windowEndTime, setWindowEndTimeUnthrottled] = useState<Date>();
  const setWindowStartTime = useCallback(
    _.throttle(setWindowStartTimeUnthrottled, 500, { trailing: false }),
    [],
  );
  const setWindowEndTime = useCallback(
    _.throttle(setWindowEndTimeUnthrottled, 500, { trailing: false }),
    [],
  );
  const setPlayerTime = _.throttle(
    (time: Date) => playerControls?.setPlayerTime(time),
    200,
    { trailing: false },
  );

  const minZoom = 2;
  const maxZoom = 1600;

  // X position of visible window relative to browser
  const windowStartX = useRef<number>(0);
  // X position of how far it's scrolled from the beginning
  const windowScrollX = useRef<number>(0);
  const windowLockInterval = useRef<NodeJS.Timeout>();

  const pixelsPerMinute = PIXEL_ZOOM_FACTOR * zoomLevel;
  const goToTime = useCallback(
    (time: Date) => {
      centerWindow(
        spectrogramWindow,
        time,
        timelineStartTime,
        pixelsPerMinute,
        setWindowStartTime,
        setWindowEndTime,
        playerControls,
      );
    },
    [
      spectrogramWindow,
      timelineStartTime,
      pixelsPerMinute,
      playerControls,
      setWindowStartTime,
      setWindowEndTime,
    ],
  );

  if (spectrogramControls) {
    spectrogramControls.current = {
      goToTime,
      zoomIn: () => setZoomLevel((zoom) => _.clamp(zoom * 2, minZoom, maxZoom)),
      zoomOut: () =>
        setZoomLevel((zoom) => _.clamp(zoom / 2, minZoom, maxZoom)),
    };
  }

  useEffect(() => {
    if (spectrogramWindow.current) {
      setWindowStartTime(
        offsetToTime(
          spectrogramWindow.current.scrollLeft,
          timelineStartTime,
          pixelsPerMinute,
        ),
      );
      setWindowEndTime(
        offsetToTime(
          spectrogramWindow.current.scrollLeft +
            spectrogramWindow.current.clientWidth,
          timelineStartTime,
          pixelsPerMinute,
        ),
      );
    }
  }, [
    spectrogramWindow,
    setWindowStartTime,
    setWindowEndTime,
    timelineStartTime,
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
            timelineStartTime,
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
    timelineStartTime,
    setWindowStartTime,
    setWindowEndTime,
  ]);

  const handleTouchStart = (e: React.TouchEvent) => {
    setIsDragging(true);
    windowStartX.current =
      e.touches[0].pageX - (spectrogramWindow.current?.offsetLeft ?? 0);
    windowScrollX.current = spectrogramWindow.current?.scrollLeft ?? 0;
    playerControls?.pause();
  };

  const handleTouchMove = (e: React.TouchEvent) => {
    if (!isDragging || !spectrogramWindow.current) return;
    e.preventDefault();
    const containerCursorX =
      e.touches[0].pageX - spectrogramWindow.current.offsetLeft;
    const move = containerCursorX - windowStartX.current;
    const offset = windowScrollX.current - move;
    spectrogramWindow.current.scrollLeft = offset;
    const windowWidth = spectrogramWindow.current.offsetWidth;
    const targetTime = offsetToTime(
      offset + windowWidth / 2,
      timelineStartTime,
      pixelsPerMinute,
    );
    setPlayerTime(targetTime);
    setWindowStartTime(
      offsetToTime(windowScrollX.current, timelineStartTime, pixelsPerMinute),
    );
    setWindowEndTime(
      offsetToTime(
        windowScrollX.current + spectrogramWindow.current.offsetWidth,
        timelineStartTime,
        pixelsPerMinute,
      ),
    );
  };

  const handleTouchEnd = useCallback(() => {
    setIsDragging(false);
  }, []);

  const handleMouseDown = (e: React.MouseEvent) => {
    setIsDragging(true);
    windowStartX.current =
      e.pageX - (spectrogramWindow.current?.offsetLeft ?? 0);
    windowScrollX.current = spectrogramWindow.current?.scrollLeft ?? 0;
    playerControls?.pause();
  };

  const handleMouseLeave = useCallback(() => {
    setIsDragging(false);
  }, []);

  const handleMouseUp = useCallback(() => {
    setIsDragging(false);
  }, []);

  const handleMouseMove = (e: React.MouseEvent) => {
    if (!isDragging || !spectrogramWindow.current) return;
    e.preventDefault();
    const containerCursorX = e.pageX - spectrogramWindow.current.offsetLeft;
    const move = containerCursorX - windowStartX.current;
    const offset = windowScrollX.current - move;
    spectrogramWindow.current.scrollLeft = offset;
    const windowWidth = spectrogramWindow.current.offsetWidth;
    const targetTime = offsetToTime(
      offset + windowWidth / 2,
      timelineStartTime,
      pixelsPerMinute,
    );
    playerTimeRef.current = targetTime;

    setPlayerTime(targetTime);

    setWindowStartTime(
      offsetToTime(windowScrollX.current, timelineStartTime, pixelsPerMinute),
    );
    setWindowEndTime(
      offsetToTime(
        windowScrollX.current + spectrogramWindow.current.offsetWidth,
        timelineStartTime,
        pixelsPerMinute,
      ),
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
        position="relative"
        minHeight={SPECTROGRAM_HEIGHT}
        bgcolor="#efefef"
        width={"100%"}
        display="flex"
        sx={{
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
        onMouseDown={handleMouseDown}
        onMouseMove={handleMouseMove}
        onTouchStart={handleTouchStart}
        onTouchMove={handleTouchMove}
        onTouchEnd={handleTouchEnd}
        onTouchCancel={handleTouchEnd}
      >
        <Box
          borderLeft="2px solid #eee"
          position="sticky"
          left="50%"
          width={"1px"}
          zIndex={4}
          sx={{ transform: `translateY(${TICKER_HEIGHT}px)` }}
        ></Box>

        {boutStartTime && (
          <TimelineMarker
            time={boutStartTime}
            pixelsPerMinute={pixelsPerMinute}
            timelineStartTime={timelineStartTime}
            zIndex={4}
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
            timelineStartTime={timelineStartTime}
            zIndex={4}
            Icon={PlayCircleFilled}
            iconProps={{ transform: "scaleX(-1)" }}
            onClick={() => {
              goToTime(boutEndTime);
            }}
          />
        )}

        <FrequencyAxisLayer
          minFrequency={1}
          maxFrequency={48000}
          scaling="logarithmic"
          zIndex={3}
        />

        {windowStartTime && windowEndTime && (
          <TimelineTickerLayer
            timelineStartTime={timelineStartTime}
            timelineEndTime={timelineEndTime}
            windowStartTime={windowStartTime}
            windowEndTime={windowEndTime}
            pixelsPerMinute={pixelsPerMinute}
            zIndex={3}
          />
        )}
        <BaseAudioWidthLayer
          startTime={timelineStartTime}
          endTime={timelineEndTime}
          pixelsPerMinute={pixelsPerMinute}
          zIndex={1}
        />
        {windowStartTime && windowEndTime && (
          <FeedSegmentsLayer
            feedSegments={feedSegments}
            timelineStartTime={timelineStartTime}
            pixelsPerMinute={pixelsPerMinute}
            windowStartTime={windowStartTime}
            windowEndTime={windowEndTime}
            zIndex={2}
          />
        )}
      </Box>
    </>
  );
}
