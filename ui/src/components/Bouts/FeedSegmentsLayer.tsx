import { Box, Typography } from "@mui/material";

import { AudioImage } from "@/graphql/generated";

import {
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
}: {
  feedSegments: SpectrogramFeedSegment[];
  timelineStartTimeNum: number;
  pixelsPerMinute: number;
  zIndex: number;
}) {
  const timelineStartTime = new Date(timelineStartTimeNum);

  return (
    <>
      {feedSegments.flatMap((feedSegment) => {
        if (
          feedSegment.startTime !== undefined &&
          feedSegment.startTime !== null &&
          feedSegment.endTime !== undefined
        ) {
          if (feedSegment.audioImages[0]) {
            return [];
          }

          const startTime = new Date(feedSegment.startTime);
          const offset = timeToOffset(
            startTime,
            timelineStartTime,
            pixelsPerMinute,
          );
          const width = (pixelsPerMinute * Number(feedSegment.duration)) / 60;
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
              }}
              display="flex"
              alignItems="center"
              justifyContent="center"
              data-starttime={feedSegment.startTime}
              data-endtime={feedSegment.endTime}
              data-duration={feedSegment.duration}
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
              >
                {startTime?.toLocaleTimeString()}
              </Typography>
            </Box>,
          ];
        }
      })}
    </>
  );
}
