import { Box, Typography } from "@mui/material";
import {
  addMinutes,
  differenceInMilliseconds,
  differenceInMinutes,
  format,
  subMinutes,
} from "date-fns";
import { useEffect, useRef, useState } from "react";

import { AudioImage, FeedSegment } from "@/graphql/generated";

const TICKER_HEIGHT = 30;
const SPECTROGRAM_HEIGHT = 300;

function timeToOffset(
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
  "id" | "startTime" | "endTime" | "duration" | "audioImages"
>;

export default function SpectrogramTimeline({
  playerTime,
  timelineStartTime,
  timelineEndTime,
  feedSegments,
}: {
  playerTime?: Date;
  timelineStartTime: Date;
  timelineEndTime: Date;
  feedSegments: SpectrogramFeedSegment[];
}) {
  // Full spectrogram container
  const spectrogramWindow = useRef<HTMLDivElement | null>(null);
  const [spectrogramTime, setSpectrogramTime] = useState<Date>();
  const [isDragging, setIsDragging] = useState<boolean>(false);
  const [zoomLevel, setZoomLevel] = useState<number>(10);
  const [windowStartTime, setWindowStartTime] = useState<Date>();
  const [windowEndTime, setWindowEndTime] = useState<Date>();
  const minZoom = 5;
  const maxZoom = 100;

  // X position of visible window relative to browser
  const windowStartX = useRef<number>(0);

  // X position of how far it's scrolled from the beginning
  const windowScrollX = useRef<number>(0);

  const pixelsPerMinute = 50 * zoomLevel;

  useEffect(() => {
    if (spectrogramTime === undefined && playerTime !== undefined) {
      // Set initial spectrogram time
      setSpectrogramTime(playerTime);
    }
    if (
      windowStartTime === undefined &&
      windowScrollX.current !== undefined &&
      timelineStartTime !== undefined
    ) {
      setWindowStartTime(
        offsetToTime(windowScrollX.current, timelineStartTime, pixelsPerMinute),
      );
    }
    if (
      windowEndTime === undefined &&
      spectrogramWindow.current !== undefined &&
      spectrogramWindow.current !== null &&
      windowScrollX.current !== undefined &&
      timelineStartTime !== undefined
    ) {
      setWindowEndTime(
        offsetToTime(
          windowScrollX.current + spectrogramWindow.current.offsetWidth,
          timelineStartTime,
          pixelsPerMinute,
        ),
      );
    }
  }, [
    playerTime,
    spectrogramTime,
    spectrogramWindow,
    pixelsPerMinute,
    timelineStartTime,
    windowStartTime,
    windowEndTime,
  ]);

  useEffect(() => {
    setWindowStartTime(
      offsetToTime(windowScrollX.current, timelineStartTime, pixelsPerMinute),
    );
    setWindowEndTime(
      offsetToTime(
        windowScrollX.current + (spectrogramWindow.current?.offsetWidth ?? 0),
        timelineStartTime,
        pixelsPerMinute,
      ),
    );
  }, [windowScrollX, spectrogramWindow, timelineStartTime, pixelsPerMinute]);

  const handleTouchStart = (e: React.TouchEvent) => {
    setIsDragging(true);
    windowStartX.current =
      e.touches[0].pageX - (spectrogramWindow.current?.offsetLeft ?? 0);
    windowScrollX.current = spectrogramWindow.current?.scrollLeft ?? 0;
  };

  const handleTouchMove = (e: React.TouchEvent) => {
    if (!isDragging || !spectrogramWindow.current) return;
    e.preventDefault();
    const containerCursorX =
      e.touches[0].pageX - spectrogramWindow.current.offsetLeft;
    const move = containerCursorX - windowStartX.current;
    spectrogramWindow.current.scrollLeft = windowScrollX.current - move;
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
    windowStartX.current =
      e.pageX - (spectrogramWindow.current?.offsetLeft ?? 0);
    windowScrollX.current = spectrogramWindow.current?.scrollLeft ?? 0;
  };

  const handleMouseLeave = () => {
    setIsDragging(false);
  };

  const handleMouseUp = () => {
    setIsDragging(false);
  };

  const handleMouseMove = (e: React.MouseEvent) => {
    if (!isDragging || !spectrogramWindow.current) return;
    e.preventDefault();
    const containerCursorX = e.pageX - spectrogramWindow.current.offsetLeft;
    const move = containerCursorX - windowStartX.current;
    spectrogramWindow.current.scrollLeft = windowScrollX.current - move;
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
    setZoomLevel((zoom) => {
      const zoomIncrement = zoom * 0.2;
      return Math.min(
        Math.max(
          minZoom,
          zoom + (e.deltaY > 0 ? -zoomIncrement : zoomIncrement),
        ),
        maxZoom,
      );
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
  }, []);

  return (
    <>
      <div>Start: {JSON.stringify(timelineStartTime)}</div>
      <div>End: {JSON.stringify(timelineEndTime)}</div>
      <div>Window start: {JSON.stringify(windowStartTime)}</div>
      <div>Window end: {JSON.stringify(windowEndTime)}</div>
      <div>Zoom {zoomLevel}</div>
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
        }}
        onMouseDown={handleMouseDown}
        onMouseMove={handleMouseMove}
        onTouchStart={handleTouchStart}
        onTouchMove={handleTouchMove}
        onWheel={handleWheel}
      >
        {/* <PlayHeadLayer
          playerTime={playerTime ?? spectrogramTime}
          timelineStartTime={timelineStartTime}
          timelineEndTime={timelineEndTime}
          spectrogramWindow={spectrogramWindow}
          zIndex={5}
        /> */}
        <TimelineTickerLayer
          startTime={timelineStartTime}
          endTime={timelineEndTime}
          pixelsPerMinute={pixelsPerMinute}
          zIndex={1}
        />
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
      ({JSON.stringify(spectrogramTime)} / {JSON.stringify(playerTime)})
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
            bgcolor={"#ddd"}
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
                    subMinutes(startTime, 500 / pixelsPerMinute),
                    addMinutes(endTime, 500 / pixelsPerMinute),
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

function TimelineTickerLayer({
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
  const tiles = minutes; // 1 minute increments
  return (
    <>
      {Array(tiles)
        .fill(0)
        .map((_, idx) => (
          <Box
            key={idx}
            zIndex={zIndex}
            bgcolor={"#ddd"}
            display="flex"
            borderBottom="1px solid #aaa"
            sx={{
              minWidth: pixelsPerMinute,
              height: TICKER_HEIGHT,
              position: "absolute",
              left: idx * pixelsPerMinute,
            }}
          >
            <Box position="relative" width="100%">
              <Box
                position="absolute"
                left="0"
                height="35%"
                borderLeft="1px solid #666"
                borderRight="1px solid #aaa"
                bottom="0"
              />
              <Box
                position="absolute"
                left="50%"
                bottom="0"
                height="30%"
                borderLeft="1px solid #666"
                borderRight="1px solid #aaa"
              />
              {[1, 2, 4, 5].map((tensIndex) => (
                <Box
                  key={tensIndex}
                  position="absolute"
                  left={`${(100 * tensIndex) / 6}%`}
                  bottom="0"
                  height="20%"
                  borderLeft="1px solid #666"
                  borderRight="1px solid #aaa"
                />
              ))}
              <Box
                position="absolute"
                left="-50%"
                width="100%"
                display="flex"
                justifyContent="center"
                height="50%"
                top={0}
              >
                <Typography
                  fontSize={14}
                  fontWeight="semibold"
                  width="100%"
                  textAlign="center"
                  lineHeight={1}
                  p={0}
                  mx={0}
                  mb={0}
                  mt={"2px"}
                >
                  {format(addMinutes(startTime, idx), "hh:mm:ss")}
                </Typography>
              </Box>
            </Box>
          </Box>
        ))}
    </>
  );
}

function PlayHeadLayer({
  playerTime,
  timelineStartTime,
  pixelsPerMinute,
  zIndex,
}: {
  playerTime: Date;
  // spectrogramWindow: MutableRefObject<HTMLElement | null>;
  timelineStartTime: Date;
  pixelsPerMinute: number;
  zIndex: number;
}) {
  const offset = timeToOffset(playerTime, timelineStartTime, pixelsPerMinute);

  return (
    <>
      <Box
        borderLeft="2px solid #eee"
        position="absolute"
        left={offset}
        top={TICKER_HEIGHT}
        height={`calc(100% - ${TICKER_HEIGHT}px)`}
        width={1}
        zIndex={zIndex}
      ></Box>
    </>
  );
}
