import { Launch, PlayArrow } from "@mui/icons-material";
import {
  Box,
  Chip,
  IconButton,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  Typography,
} from "@mui/material";
import { format } from "date-fns";
import { MutableRefObject } from "react";

import { Candidate, Detection, Maybe } from "@/graphql/generated";

import { PlayerControls } from "../Player/BoutPlayer";
import { SpectrogramControls } from "./SpectrogramTimeline";

export function BoutDetectionsTable({
  detections,
  minDetectionsTime,
  maxDetectionsTime,
  spectrogramControls,
  playerControls,
}: {
  detections: Array<
    Pick<Detection, "id" | "category" | "timestamp" | "description"> & {
      candidate?: Maybe<Pick<Candidate, "id">>;
    }
  >;
  minDetectionsTime: Date;
  maxDetectionsTime: Date;
  playerControls: MutableRefObject<PlayerControls | undefined>;
  spectrogramControls: MutableRefObject<SpectrogramControls | undefined>;
}) {
  return (
    <Box sx={{ overflowX: "auto" }}>
      <Table>
        <TableHead>
          <TableRow>
            <TableCell></TableCell>
            <TableCell>#</TableCell>
            <TableCell>ID</TableCell>
            <TableCell>Category</TableCell>
            <TableCell>Description</TableCell>
            <TableCell align="right">Timestamp</TableCell>
            <TableCell align="right">Candidate</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {detections
            .sort(({ timestamp: a }, { timestamp: b }) => {
              const date_a = new Date(a);
              const date_b = new Date(b);
              // Sort by timestamp, low to high
              return +date_a - +date_b;
            })
            .map((det, index) => (
              <TableRow key={index}>
                <TableCell>
                  <IconButton
                    size="small"
                    sx={{ transform: "scale(0.8)" }}
                    onClick={() => {
                      spectrogramControls.current?.goToTime(
                        new Date(det.timestamp),
                      );
                      playerControls.current?.play();
                    }}
                  >
                    <PlayArrow />
                  </IconButton>
                </TableCell>
                <TableCell>{index + 1}</TableCell>
                <TableCell>
                  <Typography variant="caption">{det.id}</Typography>
                </TableCell>
                <TableCell>
                  <Chip label={det.category} />
                </TableCell>
                <TableCell>{det.description}</TableCell>
                <TableCell
                  align="right"
                  title={new Date(det.timestamp).toString()}
                >
                  {format(new Date(det.timestamp), "h:mm:ss a O")}
                </TableCell>
                <TableCell align="right">
                  {det?.candidate?.id && (
                    <IconButton
                      href={`/reports/${det?.candidate?.id}`}
                      target="_blank"
                      size="small"
                      sx={{ transform: "scale(0.8)" }}
                    >
                      <Launch />
                    </IconButton>
                  )}
                </TableCell>
              </TableRow>
            ))}

          {detections.length < 1 && (
            <TableRow>
              <TableCell colSpan={5}>
                <Typography textAlign="center">
                  No detections submitted from{" "}
                  {format(minDetectionsTime, "h:mm a O")} to{" "}
                  {format(maxDetectionsTime, "h:mm a O")}
                </Typography>
              </TableCell>
            </TableRow>
          )}
        </TableBody>
      </Table>
    </Box>
  );
}
