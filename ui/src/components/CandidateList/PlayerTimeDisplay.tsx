import { Box, List, ListItem } from "@mui/material";
import { MutableRefObject, useEffect, useState } from "react";

import { useNowPlaying } from "@/context/NowPlayingContext";
import { useComputedPlaybackFields } from "@/hooks/useComputedPlaybackFields";
import { CombinedData } from "@/types/DataTypes";
import {
  addMilliseconds,
  subtractMilliseconds,
} from "@/utils/masterDataHelpers";

export default function PlayerTimeDisplay({
  masterPlayerTimeRef,
}: {
  masterPlayerTimeRef: MutableRefObject<number>;
}) {
  const { nowPlayingCandidate } = useNowPlaying();
  const { startOffset } = useComputedPlaybackFields(nowPlayingCandidate);

  const [currentDetections, setCurrentDetections] = useState<
    CombinedData[] | undefined
  >(undefined);
  const startTime = nowPlayingCandidate?.startTimestamp; // ISO timestamp for the start of the candidate

  useEffect(() => {
    let frameId: number;

    const update = () => {
      const detections = nowPlayingCandidate?.array;
      const masterPlayerTime = masterPlayerTimeRef.current;
      if (!startTime) {
        return;
      }

      const currentSeconds = (masterPlayerTime - startOffset) * 1000; // seconds from the start
      const currentTimestamp = startTime
        ? addMilliseconds(startTime, currentSeconds)
        : "";
      if (!currentTimestamp) return;

      const currentRange = [
        subtractMilliseconds(currentTimestamp, 15000),
        addMilliseconds(currentTimestamp, 15000),
      ];

      const detectionsInRange = detections?.filter((d) => {
        return (
          d.timestampString >= currentRange[0] &&
          d.timestampString <= currentRange[1]
        );
      });

      setCurrentDetections(detectionsInRange);
      frameId = requestAnimationFrame(update);
    };

    update();

    return () => cancelAnimationFrame(frameId);
  }, [masterPlayerTimeRef, nowPlayingCandidate, startOffset, startTime]);

  return (
    <Box>
      {
        <List
          id="report-list"
          sx={{
            paddingTop: "2px",
          }}
        >
          {currentDetections &&
            currentDetections.map((d) => {
              const detectionTimestamp = new Date(d.timestampString);
              const detectionTime = detectionTimestamp.toLocaleString(
                undefined,
                {
                  hour: "2-digit",
                  minute: "2-digit",
                  second: "2-digit",
                  // timeZoneName: "short",
                },
              );

              return (
                <ListItem key={d.id} id={d.timestampString} sx={{ m: 0, p: 0 }}>
                  <mark
                    style={{
                      marginTop: "6px",
                      padding: "6px 12px",
                      background: "rgba(0,0,0,.9)",
                      borderRadius: "4px",
                      color: "white",
                    }}
                  >
                    <span>{detectionTime}</span>
                    {" " + d.newCategory + " "}
                    {d.comments}
                  </mark>
                </ListItem>
              );
            })}
        </List>
      }
    </Box>
  );
}
