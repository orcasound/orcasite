import {
  Box,
  List,
  ListItem,
  Theme,
  Typography,
  useMediaQuery,
} from "@mui/material";
import { MutableRefObject, useEffect, useState } from "react";

import { useData } from "@/context/DataContext";
import { Candidate, CombinedData } from "@/types/DataTypes";
import { formatTimestamp } from "@/utils/time";

export default function PlayerTimeDisplay(props: {
  masterPlayerTimeRef: MutableRefObject<number>;
  nowPlaying: Candidate;
  startOffset: number;
}) {
  const [displayTimestamp, setDisplayTimestamp] = useState("");
  const [currentDetections, setCurrentDetections] = useState<
    CombinedData[] | undefined
  >(undefined);
  const startTime = props.nowPlaying?.startTimestamp; // ISO timestamp for the start of the candidate

  const feedId = props.nowPlaying?.feedId;
  const { feeds } = useData();
  const feed = feeds.find((feed) => feed.id === feedId);

  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

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

  const formattedSeconds = (seconds: number) => {
    const mm = Math.floor(seconds / 60);
    const ss = seconds % 60;
    return `${Number(mm).toString().padStart(2, "0")}:${ss
      .toFixed(0)
      .padStart(2, "0")}`;
  };

  const getDetectionTime = (dateString: string, startTime: string) => {
    const detectionTime = new Date(dateString).getTime();
    const zeroTime = new Date(startTime).getTime();
    const seconds = detectionTime - zeroTime;
    return formattedSeconds(seconds / 1000);
  };

  useEffect(() => {
    let frameId: number;

    const update = () => {
      const detections = props.nowPlaying?.array;
      const masterPlayerTime = props.masterPlayerTimeRef.current;
      if (!startTime) {
        return;
      }

      const currentSeconds = (masterPlayerTime - props.startOffset) * 1000; // seconds from the start
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
  }, [props.masterPlayerTimeRef, props.nowPlaying, props.startOffset]);

  return (
    <Box>
      {!mdDown && (
        <Typography id="map-title">
          <mark
            style={{
              backgroundColor: "rgba(0,0,0,.95)",
              color: "white",
              padding: "4px 12px",
              fontSize: mdDown ? "20px" : "24px",
              borderRadius: "4px",
              width: "100%",
              display: "block",
            }}
          >
            {displayTimestamp}
          </mark>
        </Typography>
      )}
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
                <ListItem key={d.id} id={d.timestampString} sx={{ m: 0, p: 0 }}>
                  <mark
                    style={{
                      // display: "inline-block",
                      marginTop: "6px",
                      padding: "4px 12px",
                      background: "rgba(0,0,0,.9)",
                      borderRadius: "4px",
                      color: "white",
                      // opacity: 0.9,
                    }}
                  >
                    <span>
                      {getDetectionTime(d.timestampString, startTime)}
                    </span>
                    {" " + d.newCategory + " "}
                    {d.description !== null && d.description !== undefined
                      ? d.description
                      : d.comments
                        ? d.comments
                        : ""}
                  </mark>
                </ListItem>
              );
            })}
        </List>
      }
    </Box>
  );
}
