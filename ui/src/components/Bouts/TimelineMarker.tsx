import { SvgIconComponent } from "@mui/icons-material";
import { Box } from "@mui/material";

import { TICKER_HEIGHT, timeToOffset } from "./SpectrogramTimeline";

export function TimelineMarker({
  time,
  timelineStartTimeNum,
  pixelsPerMinute,
  zIndex,
  Icon,
  iconProps,
  onClick,
}: {
  time: Date;
  timelineStartTimeNum: number;
  pixelsPerMinute: number;
  zIndex: number;
  Icon: SvgIconComponent;
  iconProps?: { [key: string]: string };
  onClick?: () => void;
}) {
  // Optimize rerenders
  const timelineStartTime = new Date(timelineStartTimeNum);
  const offset = timeToOffset(time, timelineStartTime, pixelsPerMinute);
  return (
    <Box
      height={1}
      title={time.toLocaleTimeString(undefined, { timeZoneName: "short" })}
      sx={{
        position: "absolute",
        left: `${offset}px`,
        zIndex: zIndex,
        cursor: "pointer",
      }}
      onClick={onClick}
    >
      <Icon
        sx={{
          height: `calc(${TICKER_HEIGHT}px - 5px)`,
          position: "relative",
          left: "calc(-50% + 2px)",
          color: (theme) => theme.palette.accent4.light,
          ...iconProps,
        }}
      />
      <Box
        sx={{
          borderLeft: (theme) => `2px solid ${theme.palette.accent4.light}`,
          top: TICKER_HEIGHT,
          position: "absolute",
          height: `calc(100% - ${TICKER_HEIGHT}px)`,
          zIndex: zIndex - 2,
        }}
      ></Box>
    </Box>
  );
}
