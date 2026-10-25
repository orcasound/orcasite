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
  const displayBuffer = 1500; // pixels

  const windowStartTimeMinusBuffer = subMilliseconds(
    windowStartTime,
    1000 * 60 * (displayBuffer / pixelsPerMinute),
  );
  const windowEndTimePlusBuffer = addMilliseconds(
    windowEndTime,
    1000 * 60 * (displayBuffer / pixelsPerMinute),
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
                width: width,
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
              width: width,
              transformOrigin: 0,
              transform: "unset",
            }}
          />,
        ];
      })}
    </>
  );
}
