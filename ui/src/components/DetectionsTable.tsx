import {
  Box,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
} from "@mui/material";

import { Detection, Feed } from "@/graphql/generated";
import { formatTimestamp } from "@/utils/time";

import { DetectionsPlayer } from "./Player/DetectionsPlayer";

export default function DetectionsTable({
  detections,
  feed,
}: {
  detections: Detection[];
  feed: Feed;
}) {
  const offsetPadding = 5;
  const minOffset = Math.min(...detections.map((d) => +d.playerOffset));
  const maxOffset = Math.max(...detections.map((d) => +d.playerOffset));
  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;
  return (
    <Box>
      <DetectionsPlayer
        feed={feed}
        marks={detections
          .sort((a, b) => a.id.localeCompare(b.id))
          .map((d, index) => ({
            label: (index + 1).toString(),
            value: Number((+d.playerOffset - +startOffset).toFixed(1)),
          }))}
        timestamp={Math.min(...detections.map((d) => d.playlistTimestamp))}
        startOffset={startOffset}
        endOffset={endOffset}
      />
      <Table>
        <TableHead>
          <TableRow>
            <TableCell>#</TableCell>
            <TableCell>ID</TableCell>
            <TableCell>Node</TableCell>
            <TableCell>Listeners</TableCell>
            <TableCell>Description</TableCell>
            <TableCell align="right">Timestamp</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {detections
            .slice()
            .sort((a, b) => a.id.localeCompare(b.id))
            .map((detection, index) => (
              <TableRow key={detection.id} hover={true}>
                <TableCell>{index + 1}</TableCell>
                <TableCell>{detection.id}</TableCell>
                <TableCell>{feed.slug}</TableCell>
                <TableCell>{detection.listenerCount}</TableCell>
                <TableCell>{detection.description}</TableCell>
                <TableCell align="right" title={detection.timestamp.toString()}>
                  {formatTimestamp(detection.timestamp)}
                </TableCell>
              </TableRow>
            ))}
        </TableBody>
      </Table>
    </Box>
  );
}
