import { Tooltip } from "@mui/material";
import React, { MutableRefObject, useMemo } from "react";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import formatDuration from "@/utils/masterDataHelpers";
import { formatTimestamp } from "@/utils/time";

import { useComputedPlaybackFields } from "../../hooks/beta/useComputedPlaybackFields";
import { CandidatePlayer } from "./CandidatePlayer";
import LivePlayer from "./LivePlayer";

export default function PlayBar({
  masterPlayerTimeRef,
}: {
  masterPlayerTimeRef?: MutableRefObject<number>;
}) {
  // const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  const { nowPlayingCandidate, nowPlayingFeed } = useNowPlaying();

  const detections = nowPlayingCandidate?.array;
  const hydrophone = nowPlayingCandidate?.hydrophone;

  const { feeds } = useData();
  const CandidateFeed = feeds.find(
    (feed) => feed.id === nowPlayingCandidate?.feedId,
  );

  const { playlistTimestamp, playlistStartTime, startOffset, endOffset } =
    useComputedPlaybackFields(nowPlayingCandidate);

  // // skip the track if there is no audio -- this was causing strange rendering behavior
  // useEffect(() => {
  //   if(!nowPlaying) return
  //   const duration = endOffset - startOffset;
  //   const currentIndex = queue.findIndex(
  //   (candidate) => candidate.id === nowPlaying?.id,
  // );
  // const nextIndex = currentIndex + 1;
  // if (duration === 0) {
  //       setNowPlaying(queue[nextIndex]);
  // }
  // }, [nowPlaying, setNowPlaying])

  const clipDateTime = useMemo(() => {
    if (nowPlayingCandidate && nowPlayingCandidate.startTimestamp) {
      const timestamp = new Date(nowPlayingCandidate?.startTimestamp);
      return formatTimestamp(timestamp);
    } else {
      return "";
    }
  }, [nowPlayingCandidate]);

  function calcMarkValue(
    playlistStartTime: string | Date,
    startOffset: number,
    detectionTimestamp: string | Date,
  ): number {
    const startTime =
      typeof playlistStartTime === "string"
        ? new Date(playlistStartTime)
        : playlistStartTime;
    const targetTime =
      typeof detectionTimestamp === "string"
        ? new Date(detectionTimestamp)
        : detectionTimestamp;
    const startTimeSeconds = startTime.getTime() / 1000;
    const targetTimeSeconds = targetTime.getTime() / 1000;
    const seconds = targetTimeSeconds - startTimeSeconds - startOffset;
    return +seconds.toFixed(1);
  }

  const duration = formatDuration(startOffset, endOffset);

  const marks = useMemo(() => {
    return detections?.map((d) => ({
      label: (
        <Tooltip
          title={`
            ${d.newCategory} 
            ${d.comments}`}
          arrow
          placement="bottom"
          slotProps={{
            popper: {
              modifiers: [
                {
                  name: "offset",
                  options: {
                    offset: [0, -10],
                  },
                },
              ],
            },
          }}
        >
          <span
            style={{
              width: "20px",
              display: "block",
              height: "10px",
              paddingTop: "10px",
            }}
          >
            {""}
          </span>
        </Tooltip>
      ),
      value: playlistStartTime
        ? calcMarkValue(
            playlistStartTime.toString(),
            startOffset,
            d.timestampString,
          )
        : 0,
    }));
  }, [playlistStartTime, startOffset, detections]);

  return (
    <>
      {nowPlayingCandidate && CandidateFeed && (
        <CandidatePlayer
          feed={CandidateFeed}
          image={CandidateFeed.imageUrl || ""}
          playlistTimestamp={playlistTimestamp}
          startOffset={startOffset}
          endOffset={endOffset}
          duration={duration}
          key={`${startOffset}-${endOffset}`}
          clipDateTime={clipDateTime}
          clipNode={hydrophone || ""}
          marks={marks}
          masterPlayerTimeRef={masterPlayerTimeRef}
        />
      )}
      {nowPlayingFeed && <LivePlayer currentFeed={nowPlayingFeed} />}
    </>
  );
}
