import { Box, Typography } from "@mui/material";
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
        const width = (pixelsPerMinute * duration) / (60 * 1000);
        const audioImageUrl = audioImageToUrl(audioImage);
        return [
          <Box
            key={audioImage.id}
            zIndex={zIndex}
            sx={{
              minHeight: `calc(100% - ${TICKER_HEIGHT}px)`,
              position: "absolute",
              left: offset,
              top: TICKER_HEIGHT,
              width: width,
              backgroundColor: (theme) => theme.palette.accent2.main,
              ...(audioImage.status === "complete" && {
                backgroundImage: `url('${audioImageUrl}')`,
                backgroundSize: "auto 100%",
              }),
            }}
            display="flex"
            alignItems="center"
            justifyContent="center"
            data-starttime={startTime.toLocaleTimeString()}
            data-endtime={endTime.toLocaleTimeString()}
            data-duration={duration}
            data-status={audioImage.status}
          >
            <Typography
              color="white"
              variant={"subtitle2"}
              whiteSpace="nowrap"
              sx={{
                ...(pixelsPerMinute < 450 && {
                  transform: "rotate(-90deg)",
                  zoom: 1.505,
                }),
              }}
            ></Typography>
          </Box>,
        ];
      })}
    </>
  );
}
