import { Box, Typography } from "@mui/material";
import {
  addMilliseconds,
  differenceInMilliseconds,
  format,
  hoursToMilliseconds,
  minutesToMilliseconds,
  secondsToMilliseconds,
  subMilliseconds,
} from "date-fns";

import { roundToNearest } from "@/utils/time";

import { TICKER_HEIGHT, timeToOffset } from "./SpectrogramTimeline";

export function TimelineTickerLayer({
  timelineStartTimeNum,
  pixelsPerMinute,
  windowStartTimeNum,
  windowEndTimeNum,
  zIndex,
}: {
  timelineStartTimeNum: number;
  windowStartTimeNum: number;
  windowEndTimeNum: number;
  pixelsPerMinute: number;
  zIndex: number;
}) {
  const timelineStartTime = new Date(timelineStartTimeNum);
  const windowStartTime = new Date(windowStartTimeNum);
  const windowEndTime = new Date(windowEndTimeNum);
  const windowRange = differenceInMilliseconds(windowEndTime, windowStartTime);
  const ticksPerWindow = 20;
  const labelsPerWindow = 8;

  const tickSize = windowRange / ticksPerWindow;
  const labelSize = windowRange / labelsPerWindow;
  const tickScales = [
    hoursToMilliseconds(24),
    hoursToMilliseconds(6),
    hoursToMilliseconds(3),
    hoursToMilliseconds(1),
    minutesToMilliseconds(10),
    minutesToMilliseconds(5),
    minutesToMilliseconds(1),
    secondsToMilliseconds(30),
    secondsToMilliseconds(10),
    secondsToMilliseconds(5),
    secondsToMilliseconds(1),
    secondsToMilliseconds(1 / 10),
    secondsToMilliseconds(1 / 100),
    1,
  ];
  const labelScales = [
    hoursToMilliseconds(24),
    hoursToMilliseconds(6),
    hoursToMilliseconds(3),
    hoursToMilliseconds(1),
    minutesToMilliseconds(10),
    minutesToMilliseconds(5),
    minutesToMilliseconds(1),
    secondsToMilliseconds(30),
    secondsToMilliseconds(10),
    secondsToMilliseconds(5),
    secondsToMilliseconds(1),
    secondsToMilliseconds(1 / 10),
    secondsToMilliseconds(1 / 100),
    1,
  ];

  const minScaleIndex = tickScales.findIndex(
    (num) => Math.floor(tickSize / num) > 0,
  );
  const scale = tickScales[Math.max(0, minScaleIndex - 1)];
  const pixelsPerTick = pixelsPerMinute * (scale / minutesToMilliseconds(1));

  const minLabelScaleIndex = labelScales.findIndex(
    (num) => Math.floor(labelSize / num) > 0,
  );
  const labelScale = labelScales[Math.max(0, minLabelScaleIndex - 1)];
  const pixelsPerLabel =
    pixelsPerMinute * (labelScale / minutesToMilliseconds(1));

  const windowRangeBufferFactor = 0.2;
  const tickStartTime = roundToNearest(
    subMilliseconds(windowStartTime, windowRange * windowRangeBufferFactor),
    scale,
    "ceil",
  );
  const tickEndTime = roundToNearest(
    addMilliseconds(windowEndTime, windowRange * windowRangeBufferFactor),
    scale,
    "ceil",
  );
  const tickStartOffset = timeToOffset(
    tickStartTime,
    timelineStartTime,
    pixelsPerMinute,
  );

  const ticks = differenceInMilliseconds(tickEndTime, tickStartTime) / scale;

  const labelStartTime = roundToNearest(
    subMilliseconds(windowStartTime, windowRange * windowRangeBufferFactor),
    labelScale,
    "floor",
  );
  const labelEndTime = roundToNearest(
    addMilliseconds(windowEndTime, windowRange * windowRangeBufferFactor),
    labelScale,
    "ceil",
  );
  const labelStartOffset = timeToOffset(
    labelStartTime,
    timelineStartTime,
    pixelsPerMinute,
  );

  const labels =
    differenceInMilliseconds(labelEndTime, labelStartTime) / labelScale;

  return (
    <>
      {Array(ticks)
        .fill(0)
        .map((_tick, idx) => {
          return (
            <Box
              key={idx}
              zIndex={zIndex}
              bgcolor={"#efefef"}
              display="flex"
              borderBottom="1px solid #aaa"
              sx={{
                minWidth: pixelsPerTick,
                height: TICKER_HEIGHT,
                position: "absolute",
                left: tickStartOffset + idx * pixelsPerTick,
              }}
            >
              <Box position="relative" width="100%">
                <Tick left="0" height={"35%"} />
              </Box>
            </Box>
          );
        })}
      {Array(labels)
        .fill(0)
        .map((_label, idx) => {
          return (
            <Label
              key={idx}
              zIndex={zIndex + 1}
              time={addMilliseconds(labelStartTime, idx * labelScale)}
              width={pixelsPerLabel}
              left={labelStartOffset + idx * pixelsPerLabel}
              scale={labelScale}
            />
          );
        })}
    </>
  );
}

function Tick({
  left,
  height,
}: {
  left: number | string;
  height: number | string;
}) {
  return (
    <Box
      position="absolute"
      left={left}
      bottom="0"
      height={height}
      borderLeft="1px solid #666"
      borderRight="1px solid #aaa"
    />
  );
}

function Label({
  time,
  width,
  left,
  zIndex,
  scale,
}: {
  time: Date;
  width: number | string;
  left: number | string;
  zIndex: number;
  scale: number;
}) {
  return (
    <Box
      zIndex={zIndex}
      position="absolute"
      left={left}
      width={width}
      display="flex"
      justifyContent="center"
      height={TICKER_HEIGHT / 2}
      top={0}
    >
      <Typography
        fontSize={14}
        fontWeight="semibold"
        width="100%"
        position="relative"
        textAlign="center"
        lineHeight={1}
        left="-50%"
        p={0}
        mx={0}
        mb={0}
        mt={"2px"}
      >
        {format(time, scale < 1 ? "hh:mm:ss.SS" : "hh:mm:ss")}
      </Typography>
    </Box>
  );
}
