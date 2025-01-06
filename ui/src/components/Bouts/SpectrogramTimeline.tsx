import { Box, Button } from "@mui/material";
import { addMinutes, differenceInMilliseconds } from "date-fns";
import { throttle } from "lodash";
import _ from "lodash";
import {
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
import { TimelineTickerLayer } from "./TimelineTickerLayer";

export const TICKER_HEIGHT = 30;
const SPECTROGRAM_HEIGHT = 300;
const PIXEL_ZOOM_FACTOR = 50;

function centerWindow(
  spectrogramWindow: MutableRefObject<HTMLDivElement | null>,
  targetTime: Date,
  timelineStartTime: Date,
  pixelsPerMinute: number,
  setWindowStartTime: (value: SetStateAction<Date | undefined>) => void,
  setWindowEndTime: (value: SetStateAction<Date | undefined>) => void,
) {
  if (spectrogramWindow.current) {
    const offset = timeToOffset(targetTime, timelineStartTime, pixelsPerMinute);
    spectrogramWindow.current.scrollLeft =
      offset - spectrogramWindow.current.clientWidth / 2;

    throttle(
      () => {
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
      },
      500,
      { leading: true, trailing: true },
    )();
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

export type SpectrogramFeedSegment = Pick<
  FeedSegment,
  "id" | "startTime" | "endTime" | "duration"
> & { audioImages: Pick<AudioImage, "bucket" | "objectPath">[] };

export default function SpectrogramTimeline({
  timelineStartTime,
  timelineEndTime,
  feedSegments,
  playerTimeRef,
  playerControls,
}: {
  timelineStartTime: Date;
  timelineEndTime: Date;
  feedSegments: SpectrogramFeedSegment[];
  playerTimeRef: MutableRefObject<Date>;
  playerControls?: PlayerControls;
}) {
  // Full spectrogram container
  const spectrogramWindow = useRef<HTMLDivElement | null>(null);
  const [isDragging, setIsDragging] = useState<boolean>(false);
  const [zoomLevel, setZoomLevel] = useState<number>(8);
  const [windowStartTime, setWindowStartTime] = useState<Date>();
  const [windowEndTime, setWindowEndTime] = useState<Date>();

  const minZoom = 2;
  const maxZoom = 400;

  // X position of visible window relative to browser
  const windowStartX = useRef<number>(0);
  // X position of how far it's scrolled from the beginning
  const windowScrollX = useRef<number>(0);
  const windowLockInterval = useRef<NodeJS.Timeout>();

  const pixelsPerMinute = PIXEL_ZOOM_FACTOR * zoomLevel;

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
  }, [spectrogramWindow, timelineStartTime, pixelsPerMinute]);

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
      }, 20);
    }
  }, [
    isDragging,
    playerTimeRef,
    spectrogramWindow,
    pixelsPerMinute,
    timelineStartTime,
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
    playerControls?.setPlayerTime(targetTime);
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

    playerControls?.setPlayerTime(targetTime);

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
      <Box>
        <Button
          onClick={() =>
            setZoomLevel((zoom) => _.clamp(zoom * 2, minZoom, maxZoom))
          }
        >
          Zoom in
        </Button>
        <Button
          onClick={() =>
            setZoomLevel((zoom) => _.clamp(zoom / 2, minZoom, maxZoom))
          }
        >
          Zoom out
        </Button>
      </Box>
      <Box
        ref={spectrogramWindow}
        position="relative"
        minHeight={SPECTROGRAM_HEIGHT}
        bgcolor="#efefef"
        width={"100%"}
        display="flex"
        sx={{
          overflowX: "hidden",
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
          zIndex={6}
          sx={{ transform: `translateY(${TICKER_HEIGHT}px)` }}
        ></Box>

        {windowStartTime && windowEndTime && (
          <TimelineTickerLayer
            timelineStartTime={timelineStartTime}
            timelineEndTime={timelineEndTime}
            windowStartTime={windowStartTime}
            windowEndTime={windowEndTime}
            pixelsPerMinute={pixelsPerMinute}
            zIndex={5}
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
