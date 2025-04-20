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
  TextField,
  Typography,
} from "@mui/material";
import { useState } from "react";

import {
  Candidate,
  Detection,
  Feed,
  useCancelNotificationMutation,
  useGetCurrentUserQuery,
  useNotificationsForCandidateQuery,
  useNotifyConfirmedCandidateMutation,
  useSetDetectionVisibleMutation,
} from "@/graphql/generated";
import { analytics } from "@/utils/analytics";
import { formatTimestamp } from "@/utils/time";

import CircularProgressWithLabel from "./CircularProgressWithLabel";
import { DetectionsPlayer } from "./Player/DetectionsPlayer";

export default function DetectionsTable({
  detections,
  feed,
  candidate,
  onDetectionUpdate,
}: {
  detections: Detection[];
  feed: Pick<Feed, "slug" | "nodeName" | "bucket">;
  candidate: Pick<Candidate, "id" | "visible">;
  onDetectionUpdate: () => void;
}) {
  const offsetPadding = 15;
  const minOffset = Math.min(...detections.map((d) => +d.playerOffset));
  const maxOffset = Math.max(...detections.map((d) => +d.playerOffset));
  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;

  const { currentUser } = useGetCurrentUserQuery().data ?? {};

  const setDetectionVisible = useSetDetectionVisibleMutation({
    onSuccess: onDetectionUpdate,
  });

  const notificationsQuery = useNotificationsForCandidateQuery({
    candidateId: candidate.id,
  });
  const { notificationsForCandidate: notifications } =
    notificationsQuery.data ?? {};

  const cancelNotification = useCancelNotificationMutation({
    onSuccess: () => {
      notificationsQuery.refetch();
    },
  });

  return (
    <Box>
      <DetectionsPlayer
        feed={feed}
        marks={detections
          .sort(({ timestamp: a }, { timestamp: b }) => {
            const date_a = new Date(a);
            const date_b = new Date(b);

            // Sort by timestamp, low to high
            return +date_a - +date_b;
          })
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
                <TableCell>IP</TableCell>
                <TableCell>Actions</TableCell>
              </>
            )}
          </TableRow>
        </TableHead>
        <TableBody>
          {detections
            .slice()
            .sort(({ timestamp: a }, { timestamp: b }) => {
              const date_a = new Date(a);
              const date_b = new Date(b);

              // Sort by timestamp, low to high
              return +date_a - +date_b;
            })
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
                    <TableCell>{detection.sourceIp || "-"}</TableCell>
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
                onNotification={() => notificationsQuery.refetch()}
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
                  <TableCell align="center">Progress</TableCell>
                  <TableCell align="right">Last updated</TableCell>
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
                    <TableCell>
                      <Box
                        display="flex"
                        alignItems="center"
                        justifyContent="center"
                      >
                        <Box sx={{ mr: 3 }}>
                          {notification.notifiedCount} /{" "}
                          {notification.targetCount}
                        </Box>
                        {typeof notification.progress === "number" && (
                          <CircularProgressWithLabel
                            value={notification.progress * 100}
                          />
                        )}
                      </Box>
                    </TableCell>
                    <TableCell
                      align="right"
                      title={notification.notifiedCountUpdatedAt?.toString()}
                    >
                      {notification.notifiedCountUpdatedAt &&
                        formatTimestamp(notification.notifiedCountUpdatedAt)}
                    </TableCell>
                    <TableCell
                      align="right"
                      title={notification.insertedAt.toString()}
                    >
                      {formatTimestamp(notification.insertedAt)}
                    </TableCell>
                    <TableCell align="right">
                      {notification.active && !notification.finished && (
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
          <TextField
            fullWidth
            multiline
            autoFocus
            placeholder="Message to subscribers (e.g. SRKWs heard in ...)"
            onChange={handleChange}
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
