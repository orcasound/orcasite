import { Candidate } from "@/types/DataTypes";

import { useFeedSegments } from "./useFeedSegments";
import { useFeedStreams } from "./useFeedStreams";

function calcOffsetInSeconds(
  start: string | Date,
  target: string | Date,
): number {
  const startTime = typeof start === "string" ? new Date(start) : start;
  const targetTime = typeof target === "string" ? new Date(target) : target;
  return (targetTime.getTime() - startTime.getTime()) / 1000;
}

export const useComputedPlaybackFields = (
  candidate: Candidate,
  feedId: string | undefined,
) => {
  const { startTimestamp, endTimestamp } = candidate ?? "";

  const {
    data: feedSegments,
    isLoading: feedSegmentsLoading,
    error: feedSegmentsError,
  } = useFeedSegments({
    feedId,
    startTime: startTimestamp ?? "",
    endTime: endTimestamp ?? "",
    // enabled: needsOffsets, // decided not to use this
  });

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

  const playlistTimestamp = firstPlaylistTimestamp
    ? parseInt(firstPlaylistTimestamp)
    : 0;

  const startOffset =
    stream && stream.startTime && startTimestamp && startTimestamp !== ""
      ? Math.max(0, calcOffsetInSeconds(stream.startTime, startTimestamp))
      : 0;

  let endOffset = 0;

  if (stream && stream.startTime && endTimestamp && endTimestamp !== "") {
    if (isLastSegmentInSamePlaylist) {
      endOffset = calcOffsetInSeconds(stream.startTime, endTimestamp);
    } else if (stream.endTime) {
      endOffset = calcOffsetInSeconds(stream.startTime, stream.endTime);
    }
  }

  if (!startTimestamp || !endTimestamp) {
    return {
      playlistStartTime: 0,
      playlistTimestamp: 0,
      startOffset: 0,
      endOffset: 0,
      isLoading: false,
      error: null,
    };
  }

  return {
    playlistStartTime: stream?.startTime,
    playlistTimestamp,
    startOffset,
    endOffset,
    feedSegmentsLoading,
    feedSegmentsError,
    feedStreamsLoading,
    feedStreamsError,
  };
};
