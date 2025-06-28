import "videojs-offset";

import {
  Box,
  Slider,
  Theme,
  Typography,
  useMediaQuery,
  useTheme,
} from "@mui/material";
import { MutableRefObject, ReactNode } from "react";

import { type VideoJSPlayer } from "@/components/Player/VideoJS";
import { formattedSeconds } from "@/utils/masterDataHelpers";

export function PlaybarSlider({
  marks,
  playerRef,
  playerTime,
  startOffset,
  endOffset,
}: {
  marks: { label: string | ReactNode; value: number }[] | undefined;
  playerRef: MutableRefObject<VideoJSPlayer | null>;
  playerTime: number;
  startOffset: number;
  endOffset: number;
}) {
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  // const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));
  const theme = useTheme();

  const sliderMax = endOffset - startOffset;
  const sliderValue = playerTime - startOffset;

  const handleSliderChange = (
    _e: Event,
    v: number | number[],
    _activeThumb: number,
  ) => {
    playerRef?.current?.pause();
    if (typeof v !== "number") return;
    playerRef?.current?.currentTime(v + startOffset);
  };

  const handleSliderChangeCommitted = (
    _e: Event | React.SyntheticEvent<Element, Event>,
    v: number | number[],
  ) => {
    if (typeof v !== "number") return;
    playerRef?.current?.currentTime(v + startOffset);
    playerRef?.current?.play();
  };

  return (
    <Box width={"100%"} id="slider">
      <Slider
        // valueLabelDisplay="auto"
        // valueLabelFormat={(v) => `${formattedSeconds(
        //   Number((v).toFixed(0)),
        // )}`}
        step={0.1}
        max={sliderMax}
        value={sliderValue}
        marks={marks}
        onChange={handleSliderChange}
        onChangeCommitted={handleSliderChangeCommitted}
        size="small"
        sx={{
          padding: "10px 0!important",
          display: "flex",
          flexDirection: "column",
          gap: 0,
          "& .MuiSlider-markLabel": {
            display: "block",
            top: 0,
          },
          "& .MuiSlider-mark": {
            backgroundColor: theme.palette.accent3.main,
            height: "10px",
          },
          "&.MuiSlider-root": {
            marginBottom: "0px",
          },
        }}
      />
      <Box
        id="formatted-seconds"
        sx={{
          display: smDown ? "none" : "flex",
          justifyContent: "space-between",
        }}
      >
        <Typography component="p" variant="subtitle2">
          {formattedSeconds(Number((playerTime - startOffset).toFixed(0)))}
        </Typography>
        <Typography component="p" variant="subtitle2">
          {"-" + formattedSeconds(Number((endOffset - playerTime).toFixed(0)))}
        </Typography>
      </Box>
    </Box>
  );
}
