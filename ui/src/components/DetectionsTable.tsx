import {
  Box,
  Button,
  Chip,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
} from "@mui/material";
import { useQueryClient } from "@tanstack/react-query";

import {
  Candidate,
  Detection,
  Feed,
  useCandidateQuery,
  useGetCurrentUserQuery,
  useNotifyConfirmedCandidateMutation,
  useSetDetectionVisibleMutation,
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
  candidate: Pick<Candidate, "id" | "visible">;
}) {
  const offsetPadding = 15;
  const minOffset = Math.min(...detections.map((d) => +d.playerOffset));
  const maxOffset = Math.max(...detections.map((d) => +d.playerOffset));
  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;

  const { currentUser } = useGetCurrentUserQuery().data ?? {};

  const queryClient = useQueryClient();

  const setDetectionVisible = useSetDetectionVisibleMutation({
    onSuccess: () => {
      queryClient.invalidateQueries(
        useCandidateQuery.getKey({ id: candidate.id }),
      );
    },
  });

  const notifyConfirmedCandidate = useNotifyConfirmedCandidateMutation();

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
            {currentUser?.moderator && (
              <>
                <TableCell>Status</TableCell>
                <TableCell>Actions</TableCell>
              </>
            )}
          </TableRow>
        </TableHead>
        <TableBody>
          {detections
            .slice()
            .sort((a, b) => a.id.localeCompare(b.id))
            .map((detection, index) => (
              <TableRow
                key={detection.id}
                hover={true}
                sx={{ ...(!detection.visible && { opacity: 0.5 }) }}
              >
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
                  <>
                    <TableCell>
                      <Chip
                        label={detection.visible ? "Visible" : "Hidden"}
                        variant="outlined"
                      />
                    </TableCell>
                    <TableCell>
                      <Button
                        onClick={() => {
                          setDetectionVisible.mutate({
                            id: detection.id,
                            visible: !detection.visible,
                          });
                        }}
                      >
                        {detection.visible ? "Hide" : "Show"}
                      </Button>
                    </TableCell>
                  </>
                )}
              </TableRow>
            ))}
        </TableBody>
      </Table>
    </Box>
  );
}
