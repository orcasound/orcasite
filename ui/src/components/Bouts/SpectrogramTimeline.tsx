import { Box, Button, Typography } from "@mui/material";
import {
  addMinutes,
  differenceInMilliseconds,
  differenceInMinutes,
  subMinutes,
} from "date-fns";
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

function rangesOverlap(
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

function audioImageToUrl({
  bucket,
  objectPath,
}: Pick<AudioImage, "bucket" | "objectPath">) {
  return `https://${bucket}.s3.amazonaws.com${objectPath}`;
}

type SpectrogramFeedSegment = Pick<
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
  const [wasPlaying, setWasPlaying] = useState<boolean>();
  const [zoomLevel, setZoomLevel] = useState<number>(10);
  const [windowStartTime, setWindowStartTime] = useState<Date>();
  const [windowEndTime, setWindowEndTime] = useState<Date>();
  const minZoom = 5;
  const maxZoom = 100;

  // X position of visible window relative to browser
  const windowStartX = useRef<number>(0);
  // X position of how far it's scrolled from the beginning
  const windowScrollX = useRef<number>(0);
  const windowLockInterval = useRef<NodeJS.Timeout>();
  const playHead = useRef<HTMLElement>();

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
          const offset = timeToOffset(
            playerTimeRef.current,
            timelineStartTime,
            pixelsPerMinute,
          );

          if (playHead.current) {
            playHead.current.style.width = `${offset}px`;
          }

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
    // setWasPlaying(!playerControls || !playerControls?.paused());
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

  const handleMouseDown = (e: React.MouseEvent) => {
    setIsDragging(true);
    setWasPlaying(!playerControls && !playerControls?.paused());
    windowStartX.current =
      e.pageX - (spectrogramWindow.current?.offsetLeft ?? 0);
    windowScrollX.current = spectrogramWindow.current?.scrollLeft ?? 0;
    playerControls?.pause();
  };

  const handleMouseLeave = useCallback(() => {
    setIsDragging(false);
    if (wasPlaying) {
      playerControls?.play();
    }
  }, [wasPlaying, playerControls]);

  const handleMouseUp = useCallback(() => {
    setIsDragging(false);
    if (wasPlaying) {
      playerControls?.play();
    }
  }, [wasPlaying, playerControls]);

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

  const handleWheel = (e: React.WheelEvent) => {
    e.preventDefault();
    setZoomLevel((zoomLevel) => {
      const zoomIncrement = zoomLevel * 0.2;
      const newZoom = Math.min(
        Math.max(
          minZoom,
          zoomLevel + (e.deltaY > 0 ? -zoomIncrement : zoomIncrement),
        ),
        maxZoom,
      );
      return newZoom;
    });
    // if (!spectrogramWindow.current) return;
    // spectrogramWindow.current.scrollLeft -= e.deltaY;
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
      <div>Start: {JSON.stringify(timelineStartTime)}</div>
      <div>End: {JSON.stringify(timelineEndTime)}</div>
      <div>Window start: {JSON.stringify(windowStartTime)}</div>
      <div>Window end: {JSON.stringify(windowEndTime)}</div>
      <div>Zoom {zoomLevel}</div>
      <Box>
        <Button
          onClick={() => setZoomLevel((zoom) => _.clamp(zoom * 2, 1, 200))}
        >
          Zoom in
        </Button>
        <Button
          onClick={() => setZoomLevel((zoom) => _.clamp(zoom / 2, 1, 200))}
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
        onWheel={handleWheel}
      >
        <Box
          ref={playHead}
          borderRight="2px solid #eee"
          position="absolute"
          top={TICKER_HEIGHT}
          height={`calc(100% - ${TICKER_HEIGHT}px)`}
          zIndex={5}
        ></Box>

        {windowStartTime && windowEndTime && (
          <TimelineTickerLayer
            timelineStartTime={timelineStartTime}
            windowStartTime={windowStartTime}
            windowEndTime={windowEndTime}
            startTime={timelineStartTime}
            endTime={timelineEndTime}
            pixelsPerMinute={pixelsPerMinute}
            zIndex={1}
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

function BaseAudioWidthLayer({
  startTime,
  endTime,
  pixelsPerMinute,
  zIndex,
}: {
  startTime: Date;
  endTime: Date;
  pixelsPerMinute: number;
  zIndex: number;
}) {
  const minutes = differenceInMinutes(endTime, startTime);
  const tiles = minutes * 6; // 10 second tiles
  const pixelsPerTile = pixelsPerMinute / 6;
  return (
    <>
      {Array(tiles)
        .fill(0)
        .map((_, idx) => (
          <Box
            key={idx}
            zIndex={zIndex}
            bgcolor={"#fff"}
            display="flex"
            alignItems="center"
            justifyContent="center"
            borderRight="1px solid #eee"
            sx={{
              minWidth: pixelsPerTile,
              minHeight: `calc(100% - ${TICKER_HEIGHT}px)`,
              position: "absolute",
              left: idx * pixelsPerTile,
              top: TICKER_HEIGHT,
            }}
          ></Box>
        ))}
    </>
  );
}

function FeedSegmentsLayer({
  feedSegments,
  timelineStartTime,
  pixelsPerMinute,
  zIndex,
  windowStartTime,
  windowEndTime,
}: {
  feedSegments: SpectrogramFeedSegment[];
  timelineStartTime: Date;
  pixelsPerMinute: number;
  zIndex: number;
  windowStartTime: Date;
  windowEndTime: Date;
}) {
  return (
    <>
      {feedSegments.flatMap((feedSegment) => {
        if (
          feedSegment.startTime !== undefined &&
          feedSegment.startTime !== null &&
          feedSegment.endTime !== undefined &&
          feedSegment.endTime !== null &&
          typeof feedSegment.duration === "string"
        ) {
          const startTime = new Date(feedSegment.startTime);
          const endTime = new Date(feedSegment.endTime);
          const offset = timeToOffset(
            startTime,
            timelineStartTime,
            pixelsPerMinute,
          );
          const width = (pixelsPerMinute * Number(feedSegment.duration)) / 60;
          const audioImage = feedSegment.audioImages[0];
          const audioImageUrl =
            audioImage !== undefined && audioImageToUrl(audioImage);
          return [
            <Box
              key={feedSegment.id}
              zIndex={zIndex}
              sx={{
                minHeight: `calc(100% - ${TICKER_HEIGHT}px)`,
                position: "absolute",
                left: offset,
                top: TICKER_HEIGHT,
                width: width,
                backgroundColor: (theme) => theme.palette.accent2.main,
                ...(audioImageUrl &&
                  rangesOverlap(
                    subMinutes(startTime, 1500 / pixelsPerMinute),
                    addMinutes(endTime, 1500 / pixelsPerMinute),
                    windowStartTime,
                    windowEndTime,
                  ) && {
                    backgroundImage: `url('${audioImageUrl}')`,
                    backgroundSize: "cover",
                  }),
              }}
              display="flex"
              alignItems="center"
              justifyContent="center"
              data-starttime={feedSegment.startTime}
              data-endtime={feedSegment.endTime}
              data-duration={feedSegment.duration}
            >
              <Typography color="white" variant="subtitle1">
                {!audioImageUrl && startTime?.toLocaleTimeString()}
              </Typography>
            </Box>,
          ];
        }
      })}
    </>
  );
}

function PlayHeadLayer({
  playerTimeRef,
  timelineStartTime,
  pixelsPerMinute,
  zIndex,
}: {
  playerTimeRef: MutableRefObject<Date>;
  timelineStartTime: Date;
  pixelsPerMinute: number;
  zIndex: number;
}) {
  const playHead = useRef<HTMLElement>();
  const intervalRef = useRef<NodeJS.Timeout>();
  useEffect(() => {
    clearInterval(intervalRef.current);
    intervalRef.current = setInterval(() => {
      if (playHead.current) {
        const offset = timeToOffset(
          playerTimeRef.current,
          timelineStartTime,
          pixelsPerMinute,
        );

        playHead.current.style.width = `${offset}px`;
      }
    }, 10);
  }, [pixelsPerMinute, timelineStartTime, playerTimeRef]);

  return (
    <>
      <Box
        ref={playHead}
        borderRight="2px solid #eee"
        position="absolute"
        top={TICKER_HEIGHT}
        height={`calc(100% - ${TICKER_HEIGHT}px)`}
        zIndex={zIndex}
      ></Box>
    </>
  );
}
