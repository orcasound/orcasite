import { Box, Slider } from "@mui/material";
import { addMilliseconds, differenceInMilliseconds, format } from "date-fns";
import { MutableRefObject, useCallback, useEffect, useState } from "react";

import { Detection } from "@/graphql/generated";

import { PlayerControls } from "../Player/BoutPlayer";
import { SpectrogramControls } from "./SpectrogramTimeline";

const originalError = console.error.bind(console.error);

// Suppress errors about Slider value label needing to be a number.
// The slider label works much better as a formatted date, but the slider
// prints a noisy warning (even though the formatted string works fine)
// https://stackoverflow.com/a/58789421
console.error = (...msg) =>
  !msg[2]
    .toString()
    .includes(
      "Invalid prop `value` of type `string` supplied to `MuiSliderValueLabel`",
    ) && originalError(...msg);

export default function BoutScrubBar({
  feedStreamStartTimeNum,
  detections,
  playerTimeRef,
  playerControls,
  timelineStartTimeNum,
  timelineEndTimeNum,
  spectrogramControls,
}: {
  feedStreamStartTimeNum: number;
  detections: Array<Pick<Detection, "timestamp" | "playerOffset">>;
  playerTimeRef: MutableRefObject<Date>;
  playerControls?: MutableRefObject<PlayerControls | undefined>;
  timelineStartTimeNum: number;
  timelineEndTimeNum: number;
  spectrogramControls: MutableRefObject<SpectrogramControls | undefined>;
}) {
  const timelineStartTime = new Date(timelineStartTimeNum);
  const timelineEndTime = new Date(timelineEndTimeNum);

  const [playerTime, setPlayerTime] = useState(playerTimeRef.current);

  const startOffset =
    differenceInMilliseconds(
      timelineStartTime,
      new Date(feedStreamStartTimeNum),
    ) / 1000;
  const sliderMax =
    differenceInMilliseconds(timelineEndTime, timelineStartTime) / 1000;
  const sliderValue =
    differenceInMilliseconds(playerTime, timelineStartTime) / 1000;

  const marks = detections
    .sort(({ timestamp: a }, { timestamp: b }) => {
      const date_a = new Date(a);
      const date_b = new Date(b);

      // Sort by timestamp, low to high
      return +date_a - +date_b;
    })
    .map((d, index) => ({
      label: (index + 1).toString(),
      value: Number((+d.playerOffset - +startOffset).toFixed(1)),
    }));
  // const sliderMax = endOffset - startOffset;
  // const sliderValue = playerTime - startOffset;

  useEffect(() => {
    const interval = setInterval(() => {
      if (playerTimeRef.current) {
        setPlayerTime(playerTimeRef.current);
      }
    }, 100);

    return () => clearInterval(interval);
  }, [playerTimeRef]);

  const handleSliderChange = useCallback(
    (_e: Event, v: number | number[], _activeThumb: number) => {
      playerControls?.current?.pause();
      if (typeof v !== "number") return;
      spectrogramControls.current?.goToTime(
        addMilliseconds(new Date(timelineStartTimeNum), 1000 * v),
      );
    },
    [spectrogramControls, timelineStartTimeNum, playerControls],
  );

  const handleSliderChangeCommitted = useCallback(
    (
      _e: Event | React.SyntheticEvent<Element, Event>,
      v: number | number[],
    ) => {
      if (typeof v !== "number") return;
      spectrogramControls.current?.goToTime(
        addMilliseconds(new Date(timelineStartTimeNum), 1000 * v),
      );
      playerControls?.current?.play();
    },
    [spectrogramControls, timelineStartTimeNum, playerControls],
  );
  return (
    <Box zIndex={10}>
      {timelineStartTime && timelineEndTime && (
        <Slider
          valueLabelDisplay="auto"
          valueLabelFormat={(v: number, _index: number) =>
            format(
              addMilliseconds(timelineStartTime, 1000 * v),
              "h:mm:ss.SS a O",
            )
          }
          step={0.1}
          max={sliderMax}
          value={sliderValue}
          marks={marks}
          onChange={handleSliderChange}
          onChangeCommitted={handleSliderChangeCommitted}
        />
      )}
    </Box>
  );
}
