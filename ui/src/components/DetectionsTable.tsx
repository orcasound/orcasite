import {
  Box,
  Button,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
} from "@mui/material";

import {
  Candidate,
  Detection,
  Feed,
  useGetCurrentUserQuery,
} from "@/graphql/generated";
import { analytics } from "@/utils/analytics";
import { formatTimestamp } from "@/utils/time";

import { DetectionsPlayer } from "./Player/DetectionsPlayer";

export default function DetectionsTable({
  detections,
  feed,
  candidate,
}: {
  detections: Detection[];
  feed: Pick<Feed, "slug" | "nodeName">;
  candidate: Pick<Candidate, "id">;
}) {
  const offsetPadding = 15;
  const minOffset = Math.min(...detections.map((d) => +d.playerOffset));
  const maxOffset = Math.max(...detections.map((d) => +d.playerOffset));
  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;

  const { currentUser } = useGetCurrentUserQuery().data ?? {};
  console.log("user", currentUser);

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
        onAudioPlay={() => {
          if (candidate.id) {
            analytics.reports.reportAudioPlayed(candidate.id);
          }
        }}
      />
      <Table sx={{ marginTop: 6 }}>
        <TableHead>
          <TableRow>
            <TableCell>#</TableCell>
            <TableCell>ID</TableCell>
            <TableCell>Node</TableCell>
            <TableCell>Listeners</TableCell>
            <TableCell>Category</TableCell>
            <TableCell>Description</TableCell>
            <TableCell align="right">Timestamp</TableCell>
            {currentUser?.moderator && <TableCell>Actions</TableCell>}
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
                <TableCell>{detection.category?.toLowerCase()}</TableCell>
                <TableCell>{detection.description}</TableCell>
                <TableCell align="right" title={detection.timestamp.toString()}>
                  {formatTimestamp(detection.timestamp)}
                </TableCell>
                {currentUser?.moderator && (
                  <TableCell>
                    <Button
                      onClick={() => {
                        console.log("hide", detection.id);
                      }}
                    >
                      Hide
                    </Button>
                  </TableCell>
                )}
              </TableRow>
            ))}
        </TableBody>
      </Table>
    </Box>
  );
}
