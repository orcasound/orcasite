import { Box, Typography } from "@mui/material";
import { addMinutes, subMinutes } from "date-fns";

import { AudioImage } from "@/graphql/generated";

import {
  rangesOverlap,
  SpectrogramFeedSegment,
  TICKER_HEIGHT,
  timeToOffset,
} from "./SpectrogramTimeline";

function audioImageToUrl({
  bucket,
  objectPath,
}: Pick<AudioImage, "bucket" | "objectPath">) {
  return `https://${bucket}.s3.amazonaws.com${objectPath}`;
}

export function FeedSegmentsLayer({
  feedSegments,
  timelineStartTimeNum,
  pixelsPerMinute,
  zIndex,
  windowStartTimeNum,
  windowEndTimeNum,
}: {
  feedSegments: SpectrogramFeedSegment[];
  timelineStartTimeNum: number;
  pixelsPerMinute: number;
  zIndex: number;
  windowStartTimeNum: number;
  windowEndTimeNum: number;
}) {
  const timelineStartTime = new Date(timelineStartTimeNum);
  const windowStartTime = new Date(windowStartTimeNum);
  const windowEndTime = new Date(windowEndTimeNum);

  return (
    <>
      {feedSegments.flatMap((feedSegment) => {
        if (
          feedSegment.startTime !== undefined &&
          feedSegment.startTime !== null &&
          feedSegment.endTime !== undefined &&
          feedSegment.endTime !== null &&
          typeof feedSegment.duration === "string"
        ) {
          const startTime = new Date(feedSegment.startTime);
          const endTime = new Date(feedSegment.endTime);
          const offset = timeToOffset(
            startTime,
            timelineStartTime,
            pixelsPerMinute,
          );
          const width = (pixelsPerMinute * Number(feedSegment.duration)) / 60;
          const audioImage = feedSegment.audioImages[0];
          const audioImageUrl =
            audioImage !== undefined && audioImageToUrl(audioImage);
          return [
            <Box
              key={feedSegment.id}
              zIndex={zIndex}
              sx={{
                minHeight: `calc(100% - ${TICKER_HEIGHT}px)`,
                position: "absolute",
                left: offset,
                top: TICKER_HEIGHT,
                width: width,
                backgroundColor: (theme) => theme.palette.accent2.main,
                ...(audioImageUrl &&
                  rangesOverlap(
                    subMinutes(startTime, 1500 / pixelsPerMinute),
                    addMinutes(endTime, 1500 / pixelsPerMinute),
                    windowStartTime,
                    windowEndTime,
                  ) && {
                    backgroundImage: `url('${audioImageUrl}')`,
                    backgroundSize: "auto 100%",
                  }),
              }}
              display="flex"
              alignItems="center"
              justifyContent="center"
              data-starttime={feedSegment.startTime}
              data-endtime={feedSegment.endTime}
              data-duration={feedSegment.duration}
            >
              <Typography color="white" variant="subtitle1">
                {!audioImageUrl && startTime?.toLocaleTimeString()}
              </Typography>
            </Box>,
          ];
        }
      })}
    </>
  );
}
