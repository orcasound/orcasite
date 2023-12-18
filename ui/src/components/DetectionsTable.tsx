import { Close } from "@mui/icons-material";
import {
  Box,
  Button,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  TextareaAutosize,
  Typography,
} from "@mui/material";
import { useQueryClient } from "@tanstack/react-query";
import { useState } from "react";

import {
  Candidate,
  Detection,
  Feed,
  useCancelNotificationMutation,
  useCandidateQuery,
  useGetCurrentUserQuery,
  useNotificationsForCandidateQuery,
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

  const { notificationsForCandidate: notifications } =
    useNotificationsForCandidateQuery({ candidateId: candidate.id }).data ?? {};

  const cancelNotification = useCancelNotificationMutation({
    onSuccess: () => {
      queryClient.invalidateQueries(
        useNotificationsForCandidateQuery.getKey({ candidateId: candidate.id }),
      );
    },
  });

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

      {currentUser?.moderator && (
        <Box sx={{ marginTop: 10 }}>
          <Box
            display="flex"
            justifyContent="space-between"
            alignItems="center"
          >
            <h3>Notifications</h3>
            <Box>
              <NotificationModal
                candidateId={candidate.id}
                onNotification={() =>
                  queryClient.invalidateQueries(
                    useNotificationsForCandidateQuery.getKey({
                      candidateId: candidate.id,
                    }),
                  )
                }
              />
            </Box>
          </Box>
          {!notifications && <Typography>No notifications</Typography>}
          {notifications && (
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell>Event</TableCell>
                  <TableCell>Status</TableCell>
                  <TableCell align="right">Created</TableCell>
                  <TableCell align="right">Actions</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {notifications.map((notification, index) => (
                  <TableRow key={index}>
                    <TableCell>
                      {notification.eventType?.toLowerCase()}
                    </TableCell>
                    <TableCell>
                      <Chip
                        label={notification.active ? "Active" : "Inactive"}
                        variant="outlined"
                      />
                    </TableCell>
                    <TableCell
                      align="right"
                      title={notification.insertedAt.toString()}
                    >
                      {formatTimestamp(notification.insertedAt)}
                    </TableCell>
                    <TableCell align="right">
                      {notification.active && (
                        <Button
                          onClick={() => {
                            cancelNotification.mutate({ id: notification.id });
                          }}
                        >
                          Cancel
                        </Button>
                      )}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </Box>
      )}
    </Box>
  );
}

function NotificationModal({
  candidateId,
  onNotification,
}: {
  candidateId: string;
  onNotification: () => void;
}) {
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState("");
  const [confirming, setConfirming] = useState(false);

  const handleOpen = () => {
    setOpen(true);
  };

  const handleClose = () => {
    setOpen(false);
    setMessage("");
    setConfirming(false);
  };

  const handleChange = (e: React.ChangeEvent<HTMLTextAreaElement>) =>
    setMessage(e.target.value);

  const handleSubmit = () => {
    setConfirming(true);
  };

  const handleConfirm = () => {
    notifyConfirmedCandidate.mutate({ candidateId, message });
  };

  const notifyConfirmedCandidate = useNotifyConfirmedCandidateMutation({
    onSuccess: () => {
      onNotification();
      handleClose();
    },
  });

  return (
    <>
      <Button onClick={handleOpen}>Notify subscribers</Button>
      <Dialog open={open} onClose={handleClose}>
        <DialogTitle>
          <Box
            display="flex"
            justifyContent="space-between"
            alignItems="center"
          >
            Notify subscribers
            <IconButton onClick={handleClose}>
              <Close />
            </IconButton>
          </Box>
        </DialogTitle>
        <DialogContent
          sx={{ minWidth: (theme) => theme.breakpoints.values.sm }}
        >
          <TextareaAutosize
            style={{ width: "100%", padding: "15px" }}
            autoFocus
            placeholder="Message to subscribers (e.g. SRKWs heard in ...)"
            onChange={handleChange}
            minRows={3}
          />
        </DialogContent>
        <DialogActions>
          {confirming ? (
            <Box
              display="flex"
              alignItems="center"
              sx={{ width: "100%" }}
              px={2}
            >
              <Button onClick={() => setConfirming(false)} color="primary">
                Cancel
              </Button>
              <Typography sx={{ marginLeft: "auto", marginRight: 2 }}>
                Are you sure?
              </Typography>
              <Button onClick={handleConfirm} color="error" variant="outlined">
                Send to subscribers
              </Button>
            </Box>
          ) : (
            <>
              <Button onClick={handleClose} color="primary">
                Cancel
              </Button>
              <Button
                onClick={handleSubmit}
                color="primary"
                variant="outlined"
                disabled={!message}
              >
                Submit
              </Button>
            </>
          )}
        </DialogActions>
      </Dialog>
    </>
  );
}
