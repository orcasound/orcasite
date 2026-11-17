import { Box } from "@mui/material";
import { differenceInMilliseconds } from "date-fns";

import { TICKER_HEIGHT, timeToOffset } from "./SpectrogramTimeline";

/** The grey background behind the images, from the timeline's start to its end. */
export function BaseAudioWidthLayer({
  originTimeNum,
  startTimeNum,
  endTimeNum,
  pixelsPerMinute,
  zIndex,
}: {
  /** The time at the left edge of the scrolled content */
  originTimeNum: number;
  startTimeNum: number;
  endTimeNum: number;
  pixelsPerMinute: number;
  zIndex: number;
}) {
  const startTime = new Date(startTimeNum);
  const endTime = new Date(endTimeNum);

  const left = timeToOffset(
    startTime,
    new Date(originTimeNum),
    pixelsPerMinute,
  );
  const width =
    (pixelsPerMinute * differenceInMilliseconds(endTime, startTime)) /
    (60 * 1000);
  return (
    <Box
      sx={{
        zIndex: zIndex,
        bgcolor: "#bbb",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        borderRight: "1px solid #999",
        minWidth: width,
        minHeight: `calc(100% - ${TICKER_HEIGHT}px)`,
        position: "absolute",
        left,
        top: TICKER_HEIGHT,
      }}
    ></Box>
  );
}
