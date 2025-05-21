import { Box, List, ListItem } from "@mui/material";
import { MutableRefObject, useEffect, useState } from "react";

import { Candidate, CombinedData } from "@/types/DataTypes";
import {
  addMilliseconds,
  subtractMilliseconds,
} from "@/utils/masterDataHelpers";

export default function PlayerTimeDisplay({
  startOffset = 0,
  masterPlayerTimeRef,
  nowPlaying,
}: {
  masterPlayerTimeRef: MutableRefObject<number>;
  nowPlaying: Candidate | null;
  startOffset?: number;
}) {
  // const [displayTimestamp, setDisplayTimestamp] = useState("");
  const [currentDetections, setCurrentDetections] = useState<
    CombinedData[] | undefined
  >(undefined);
  const startTime = nowPlaying?.startTimestamp; // ISO timestamp for the start of the candidate

  // const hydrophone = nowPlaying?.hydrophone;

  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  // const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  useEffect(() => {
    let frameId: number;

    const update = () => {
      const detections = nowPlaying?.array;
      const masterPlayerTime = masterPlayerTimeRef.current;
      if (!startTime) {
        return;
      }

      const currentSeconds = (masterPlayerTime - startOffset) * 1000; // seconds from the start
      const currentTimestamp = startTime
        ? addMilliseconds(startTime, currentSeconds)
        : "";
      if (!currentTimestamp) return;

      // setDisplayTimestamp(formatTimestamp(currentTimestamp));

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
  }, [masterPlayerTimeRef, nowPlaying, startOffset, startTime]);

  return (
    <Box>
      {/* {!mdDown && (
        <Typography id="map-title">
          {
            <mark
              style={{
                backgroundColor: "rgba(0,0,0,.95)",
                color: "white",
                padding: "0 12px 4px",
                fontSize: mdDown ? "20px" : "24px",
                borderRadius: "4px",
                width: "100%",
                display: "block",
              }}
            >
              <span style={{ fontSize: "16px" }}>{hydrophone}</span>
              <br />
              {displayTimestamp}
            </mark>
          }
        </Typography>
      )} */}
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
                      // display: "inline-block",
                      marginTop: "6px",
                      padding: "6px 12px",
                      background: "rgba(0,0,0,.9)",
                      borderRadius: "4px",
                      color: "white",
                      // opacity: 0.9,
                    }}
                  >
                    <span>
                      {detectionTime}
                      {/* {startTime && getTimeElapsed(d.timestampString, startTime)} */}
                    </span>
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
