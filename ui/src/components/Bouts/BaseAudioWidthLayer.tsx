import { Box } from "@mui/material";
import { differenceInMilliseconds } from "date-fns";

import { TICKER_HEIGHT } from "./SpectrogramTimeline";

export function BaseAudioWidthLayer({
  startTimeNum,
  endTimeNum,
  pixelsPerMinute,
  zIndex,
}: {
  startTimeNum: number;
  endTimeNum: number;
  pixelsPerMinute: number;
  zIndex: number;
}) {
  const startTime = new Date(startTimeNum);
  const endTime = new Date(endTimeNum);

  const minutes =
    (pixelsPerMinute * differenceInMilliseconds(endTime, startTime)) /
    (60 * 1000);
  const width = minutes * pixelsPerMinute;
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
        top: TICKER_HEIGHT,
      }}
    ></Box>
  );
}
