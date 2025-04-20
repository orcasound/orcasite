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
import _ from "lodash";
import { useState } from "react";

import {
  Bout,
  useCancelNotificationMutation,
  useNotificationsForBoutQuery,
  useNotifyLiveBoutMutation,
} from "@/graphql/generated";
import { useBoutNotificationSentSubscription } from "@/hooks/useBoutNotificationSentSubscription";
import { formatTimestamp } from "@/utils/time";

import CircularProgressWithLabel from "../CircularProgressWithLabel";

export function BoutNotifications({ bout }: { bout: Pick<Bout, "id"> }) {
  const notificationsQuery = useNotificationsForBoutQuery({
    boutId: bout.id,
  });
  const initialNotifications =
    notificationsQuery.data?.notificationsForBout ?? [];

  const updatedNotifications = useBoutNotificationSentSubscription(bout.id);
  const notifications = _.uniqBy(
    [...updatedNotifications, ...initialNotifications],
    ({ id }) => id,
  ).toSorted(
    ({ insertedAt: a }, { insertedAt: b }) =>
      new Date(a).valueOf() - new Date(b).valueOf(),
  );
  const cancelNotification = useCancelNotificationMutation({
    onSuccess: () => {
      notificationsQuery.refetch();
    },
  });
  return (
    <>
      <Box sx={{ marginTop: 1 }}>
        <Box display="flex" justifyContent="space-between" alignItems="center">
          <h3>Notifications</h3>
          <Box>
            <NotificationModal
              boutId={bout.id}
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
                  <TableCell>{notification.eventType?.toLowerCase()}</TableCell>
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
    </>
  );
}

function NotificationModal({
  boutId,
  onNotification,
}: {
  boutId: string;
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
    notifyConfirmedCandidate.mutate({ boutId, message });
  };

  const notifyConfirmedCandidate = useNotifyLiveBoutMutation({
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
