import { Box, List, ListItem, Typography } from "@mui/material";
import { MutableRefObject, useEffect, useState } from "react";

import { useData } from "@/context/DataContext";
import { Candidate, CombinedData } from "@/types/DataTypes";
import { formatTimestamp } from "@/utils/time";

import { useComputedPlaybackFields } from "./useComputedPlaybackFields";

export default function PlayerTimeDisplay(props: {
  masterPlayerTimeRef: MutableRefObject<number>;
  nowPlaying: Candidate;
}) {
  const [displayTimestamp, setDisplayTimestamp] = useState("");
  const [currentDetections, setCurrentDetections] = useState<
    CombinedData[] | undefined
  >(undefined);

  const detections = props.nowPlaying?.array;

  const { feeds } = useData();
  const feed = feeds.find((feed) => feed.id === detections?.[0]?.feedId);

  const { startOffset } = useComputedPlaybackFields(props.nowPlaying, feed?.id);

  const addMilliseconds = (dateString: string, secondsToAdd: number) => {
    const originalDate = new Date(dateString);
    originalDate.setMilliseconds(originalDate.getMilliseconds() + secondsToAdd);
    return originalDate?.toISOString();
  };

  const subtractMilliseconds = (dateString: string, secondsToAdd: number) => {
    const originalDate = new Date(dateString);
    originalDate.setMilliseconds(originalDate.getMilliseconds() - secondsToAdd);
    return originalDate?.toISOString();
  };

  useEffect(() => {
    let frameId: number;

    const update = () => {
      const detections = props.nowPlaying?.array;
      const masterPlayerTime = props.masterPlayerTimeRef.current;
      const startTime = props.nowPlaying?.startTimestamp; // ISO timestamp for the start of the candidate
      if (!startTime) {
        return;
      }

      const currentSeconds = (masterPlayerTime - startOffset) * 1000; // seconds from the start
      const currentTimestamp = startTime
        ? addMilliseconds(startTime, currentSeconds)
        : "";
      if (!currentTimestamp) return;

      setDisplayTimestamp(formatTimestamp(currentTimestamp));

      const currentRange = [
        subtractMilliseconds(currentTimestamp, 15000),
        addMilliseconds(currentTimestamp, 15000),
      ];

      const detectionsInRange = detections.filter((d) => {
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
  }, [props.masterPlayerTimeRef, props.nowPlaying, startOffset]);

  return (
    <Box>
      <Typography id="map-title">
        <mark
          style={{
            backgroundColor: "rgba(0,0,0,.95)",
            color: "white",
            padding: "4px 12px",
            fontSize: "24px",
            borderRadius: "4px",
            width: "100%",
            display: "block",
          }}
        >
          {displayTimestamp}
        </mark>
      </Typography>
      {
        <List
          id="report-list"
          sx={{
            paddingTop: "2px",
          }}
        >
          {currentDetections &&
            currentDetections.map((d) => {
              return (
                <ListItem
                  key={d.id}
                  id={d.timestampString}
                  sx={{
                    display: "inline-block",
                    marginTop: "6px",
                    padding: "4px 12px",
                    background: "rgba(0,0,0,1)",
                    borderRadius: "4px",
                    opacity: 0.9,
                  }}
                >
                  {d.newCategory + " "}
                  {d.description !== null && d.description !== undefined
                    ? d.description
                    : d.comments
                      ? d.comments
                      : ""}
                </ListItem>
              );
            })}
        </List>
      }
    </Box>
  );
}
