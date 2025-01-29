import { Box } from "@mui/material";
import { differenceInMinutes } from "date-fns";

import { TICKER_HEIGHT } from "./SpectrogramTimeline";

export function BaseAudioWidthLayer({
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
