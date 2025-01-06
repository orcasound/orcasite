import { Box, Typography } from "@mui/material";
import { addMinutes, addSeconds, differenceInMinutes, format } from "date-fns";
import _ from "lodash";

import { TICKER_HEIGHT, timeToOffset } from "./SpectrogramTimeline";

export function TimelineTickerLayer({
  startTime,
  endTime,
  pixelsPerMinute,
  timelineStartTime,
  windowStartTime,
  windowEndTime,
  zIndex,
}: {
  startTime: Date;
  endTime: Date;
  timelineStartTime: Date;
  windowStartTime: Date;
  windowEndTime: Date;
  pixelsPerMinute: number;
  zIndex: number;
}) {
  const minutes = differenceInMinutes(endTime, startTime);
  const tiles = minutes; // 1 minute increments

  const seconds = _.range(1, 59);
  const tens = seconds.filter((second) => second % 10 === 0);
  const tensTicks = _.without(tens, 30);
  const fives = seconds.filter((second) => second % 5 === 0);
  const fivesTicks = _.without(fives, ...tens);
  const onesTicks = _.without(seconds, ...tens, ...fives);

  const tensLabelThreshold = 600; // pixels per minute
  const fivesTicksThreshold = 600; // pixels per minute
  const fivesLabelThreshold = 1750; // pixels per minute
  const onesTicksThreshold = 750; // pixels per minute

  const windowStartOffset = timeToOffset(
    windowStartTime,
    timelineStartTime,
    pixelsPerMinute,
  );
  const windowEndOffset = timeToOffset(
    windowEndTime,
    timelineStartTime,
    pixelsPerMinute,
  );

  return (
    <>
      {Array(tiles)
        .fill(0)
        .map((_minute, idx) => {
          const inRange = _.inRange(
            idx * pixelsPerMinute,
            windowStartOffset - 1000,
            windowEndOffset + 1000,
          );
          // if (!inRange) return <></>;
          return (
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
                <Tick left="0" height="35%" />
                <Tick left="50%" height="30%" />
                {tensTicks.map((number, tensTickerIndex) => (
                  <Tick
                    key={tensTickerIndex}
                    left={`${(100 * number) / 60}%`}
                    height="20%"
                  />
                ))}
                {pixelsPerMinute >= tensLabelThreshold &&
                  tens.map((number, tensLabelIndex) => (
                    <Label
                      key={tensLabelIndex}
                      left={`${(100 * number) / 60}%`}
                      width={`${100 / 6}%`}
                      time={addSeconds(addMinutes(startTime, idx), number)}
                    />
                  ))}

                {pixelsPerMinute >= fivesLabelThreshold &&
                  fivesTicks.map((number, fivesLabel) => (
                    <Label
                      key={fivesLabel}
                      left={`${(100 * number) / 60}%`}
                      width={`${100 / 6}%`}
                      time={addSeconds(addMinutes(startTime, idx), number)}
                    />
                  ))}

                {pixelsPerMinute >= fivesTicksThreshold &&
                  fivesTicks.map((number, fivesTickerIdx) => (
                    <Tick
                      key={fivesTickerIdx}
                      left={`${(100 * number) / 60}%`}
                      height={"15%"}
                    />
                  ))}

                {pixelsPerMinute >= onesTicksThreshold &&
                  onesTicks.map((number, onesTicksIndex) => (
                    <Tick
                      key={onesTicksIndex}
                      left={`${(100 * number) / 60}%`}
                      height={"10%"}
                    />
                  ))}
                <Label
                  time={addMinutes(startTime, idx)}
                  left={0}
                  width={"100%"}
                />
              </Box>
            </Box>
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
}: {
  time: Date;
  width: number | string;
  left: number | string;
}) {
  return (
    <Box
      position="absolute"
      left={left}
      width={width}
      display="flex"
      justifyContent="center"
      height="50%"
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
        {format(time, "hh:mm:ss")}
      </Typography>
    </Box>
  );
}
