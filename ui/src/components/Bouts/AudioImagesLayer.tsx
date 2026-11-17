import { Box, Skeleton } from "@mui/material";
import {
  addMilliseconds,
  differenceInMilliseconds,
  subMilliseconds,
} from "date-fns";

import { AudioImage, AudioImagesQuery } from "@/graphql/generated";

import {
  rangesOverlap,
  TICKER_HEIGHT,
  timeToOffset,
} from "./SpectrogramTimeline";

function audioImageToUrl({
  bucket,
  objectPath,
}: Pick<AudioImage, "bucket" | "objectPath">) {
  return `https://${bucket}.s3.amazonaws.com${objectPath}`;
}

export function AudioImagesLayer({
  audioImages,
  timelineStartTimeNum,
  pixelsPerMinute,
  zIndex,
  windowStartTimeNum,
  windowEndTimeNum,
}: {
  audioImages: NonNullable<
    NonNullable<AudioImagesQuery["audioImages"]>["results"]
  >;
  timelineStartTimeNum: number;
  pixelsPerMinute: number;
  zIndex: number;
  windowStartTimeNum: number;
  windowEndTimeNum: number;
}) {
  const timelineStartTime = new Date(timelineStartTimeNum);
  const windowStartTime = new Date(windowStartTimeNum);
  const windowEndTime = new Date(windowEndTimeNum);
  // Images this far beyond each edge of the window are drawn too, so that
  // scrolling doesn't show a gap before the window's times catch up: 1500
  // pixels' worth, but at most five minutes, because when zoomed out 1500
  // pixels holds more images than the window does.
  const displayBufferMs = Math.min(
    1000 * 60 * (1500 / pixelsPerMinute),
    5 * 60 * 1000,
  );

  const windowStartTimeMinusBuffer = subMilliseconds(
    windowStartTime,
    displayBufferMs,
  );
  const windowEndTimePlusBuffer = addMilliseconds(
    windowEndTime,
    displayBufferMs,
  );

  return (
    <>
      {audioImages.flatMap((audioImage) => {
        const startTime = new Date(audioImage.startTime);
        const endTime = new Date(audioImage.endTime);

        if (
          !rangesOverlap(
            windowStartTimeMinusBuffer,
            windowEndTimePlusBuffer,
            startTime,
            endTime,
          )
        ) {
          return [];
        }

        const offset = timeToOffset(
          startTime,
          timelineStartTime,
          pixelsPerMinute,
        );
        const duration = differenceInMilliseconds(endTime, startTime);
        const exactWidth = (pixelsPerMinute * duration) / (60 * 1000);
        // Snap both edges to whole pixels. A tile that starts or ends mid-pixel
        // is anti-aliased, so while the timeline scrolls a hairline of the
        // background colour flickers between neighbouring tiles.
        const left = Math.round(offset);
        // A tile narrower than half a pixel would round to nothing and leave a
        // one-pixel gap between its neighbours, so give it one whole pixel.
        const width = Math.max(1, Math.round(offset + exactWidth) - left);
        const audioImageUrl = audioImageToUrl(audioImage);
        if (audioImage.status === "complete") {
          return [
            <Box
              key={audioImage.id}
              data-starttime={startTime.toLocaleTimeString()}
              data-endtime={endTime.toLocaleTimeString()}
              data-duration={duration}
              data-status={audioImage.status}
              sx={{
                zIndex: zIndex,
                display: "flex",
                alignItems: "stretch",
                justifyContent: "center",
                minHeight: `calc(100% - ${TICKER_HEIGHT}px)`,
                position: "absolute",
                left,
                top: TICKER_HEIGHT,
                // A unit is needed: MUI reads a bare width of 1 as 100%.
                width: `${width}px`,
                backgroundColor: (theme) => theme.palette.accent2.main,

                ...(audioImage.status === "complete" && {
                  backgroundImage: `url('${audioImageUrl}')`,
                  backgroundSize: "100% 100%",
                }),
              }}
            ></Box>,
          ];
        }
        return [
          <Skeleton
            key={audioImage.id}
            animation="wave"
            sx={{
              zIndex,
              borderRadius: 0,
              minHeight: `calc(100% - ${TICKER_HEIGHT}px)`,
              position: "absolute",
              left,
              top: TICKER_HEIGHT,
              // A unit is needed: MUI reads a bare width of 1 as 100%.
              width: `${width}px`,
              transformOrigin: 0,
              transform: "unset",
            }}
          />,
        ];
      })}
    </>
  );
}
