import { CombinedData } from "@/types/DataTypes";

import { useFeedSegments } from "./useFeedSegments";
import { useFeedStreams } from "./useFeedStreams";

const offsetPadding = 15;

function calcOffsetInSeconds(
  start: string | Date,
  target: string | Date,
): number {
  const startTime = typeof start === "string" ? new Date(start) : start;
  const targetTime = typeof target === "string" ? new Date(target) : target;
  return (targetTime.getTime() - startTime.getTime()) / 1000;
}

function addSecondsToTimestamp(
  timestamp: string | Date,
  seconds: number,
): string {
  const date = new Date(timestamp);
  date.setSeconds(date.getSeconds() + seconds);
  return date.toISOString();
}

export const useComputedPlaybackFields = (
  detections: CombinedData[],
  feedId: string | undefined,
) => {
  const firstDetection = detections?.[0];
  const lastDetection = detections?.[detections.length - 1];

  const firstTimestampPadded = firstDetection?.timestamp
    ? addSecondsToTimestamp(firstDetection.timestampString, -offsetPadding)
    : "";
  const lastTimestampPadded = lastDetection?.timestamp
    ? addSecondsToTimestamp(lastDetection.timestampString, offsetPadding)
    : "";

  // const needsOffsets =
  //   !firstDetection?.playlistTimestamp || !firstDetection?.playerOffset || !lastDetection.playlistTimestamp || !lastDetection.playerOffset;

  // only runs query if there isn't a playlistTimestamp or playerOffset for the first or last detection, meaning one of them is an external data source e.g. Orcahello or Cascadia, or there is missing data
  const {
    data: feedSegments,
    isLoading: feedSegmentsLoading,
    error: feedSegmentsError,
  } = useFeedSegments({
    feedId,
    startTime: firstTimestampPadded ?? "",
    endTime: lastTimestampPadded ?? "",
    // enabled: needsOffsets,
  });

  // this will be undefined if the query didn't run, meaning it already has playlistTimestamps and playerOffsets
  const segments = feedSegments?.feedSegments.results;
  const firstPlaylistTimestamp = segments?.[0]?.playlistTimestamp;
  const lastPlaylistTimestamp =
    segments?.[segments.length - 1]?.playlistTimestamp;
  const isLastSegmentInSamePlaylist =
    firstPlaylistTimestamp === lastPlaylistTimestamp;

  const {
    data: feedStreams,
    isLoading: feedStreamsLoading,
    error: feedStreamsError,
  } = useFeedStreams({
    feedId,
    playlistTimestamp: firstPlaylistTimestamp ?? "",
    // enabled: needsOffsets,
  });

  const stream = feedStreams?.feedStreams.results[0];

  // const cleanPlaylistTimestamps = (array: CombinedData[]) => {
  //   return array.map(detection => {
  //     if (detection.playlistTimestamp && typeof detection.playlistTimestamp === "number") {
  //       return detection.playlistTimestamp
  //     }
  //   })
  // }

  // if the query ran, there will be a stream.playlistTimestamp, if not it can get the earliest one from the detections
  const playlistTimestamp = firstPlaylistTimestamp
    ? parseInt(firstPlaylistTimestamp)
    : // : detections && detections.length > 0
      // ? firstDetection.playlistTimestamp
      0;

  const startOffset =
    stream && stream.startTime && firstDetection
      ? Math.max(0, calcOffsetInSeconds(stream.startTime, firstTimestampPadded))
      : // : detections && detections.length > 0
        // ? firstDetection.playerOffset - offsetPadding
        0;

  let endOffset = 0;

  if (stream?.startTime && lastDetection.timestamp) {
    if (isLastSegmentInSamePlaylist) {
      endOffset = calcOffsetInSeconds(stream.startTime, lastTimestampPadded);
    } else if (stream.endTime) {
      endOffset = calcOffsetInSeconds(stream.startTime, stream.endTime);
      // } else if (detections && detections.length > 0) {
      // endOffset = lastDetection.playerOffset + offsetPadding
    }
  }

  if (!firstDetection || !lastDetection) {
    return {
      playlistTimestamp: 0,
      startOffset: 0,
      endOffset: 0,
      isLoading: false,
      error: null,
    };
  }

  return {
    playlistTimestamp,
    startOffset,
    endOffset,
    feedSegmentsLoading,
    feedSegmentsError,
    feedStreamsLoading,
    feedStreamsError,
  };
};
