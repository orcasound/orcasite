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

export default function DetectionsTable({
  detections,
  feed,
}: {
  detections: Detection[];
  feed: Feed;
}) {
  return (
    <Box>
      <Table>
        <TableHead>
          <TableRow>
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
            .map((detection) => (
              <TableRow key={detection.id} hover={true}>
                <TableCell>{detection.id}</TableCell>
                <TableCell>{feed.slug}</TableCell>
                <TableCell>{detection.listenerCount}</TableCell>
                <TableCell>{detection.description}</TableCell>
                <TableCell
                  align="right"
                  title={detection.timestamp?.toString() || ""}
                >
                  {detection.timestamp && formatTimestamp(detection.timestamp)}
                </TableCell>
              </TableRow>
            ))}
        </TableBody>
      </Table>
    </Box>
  );
}
