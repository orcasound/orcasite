import { Box } from "@mui/material";
import { differenceInMinutes, isAfter } from "date-fns";

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

  if (!isAfter(endTime, startTime)) {
    console.warn("endTime must be after startTime");
    return null;
  }
  const minutes = differenceInMinutes(endTime, startTime);
  const tiles = minutes * 6; // 10 second tiles
  const pixelsPerTile = pixelsPerMinute / 6;
  if (minutes <= 0) {
    return null;
  }
  return (
    <>
      {Array(tiles)
        .fill(0)
        .map((_, idx) => (
          <Box
            key={idx}
            zIndex={zIndex}
            bgcolor={"#bbb"}
            display="flex"
            alignItems="center"
            justifyContent="center"
            borderRight="1px solid #999"
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
