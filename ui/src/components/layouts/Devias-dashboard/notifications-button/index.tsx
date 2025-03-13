import NotificationsNoneIcon from "@mui/icons-material/NotificationsNone";
// import NotificationsNoneIcon from '@untitled-ui/icons-react/build/esm/Bell01';
import { Badge, IconButton, SvgIcon, Tooltip } from "@mui/material";
import type { FC } from "react";
import { useCallback, useMemo, useRef, useState } from "react";

import type { Notification } from "./notifications";
import { notifications as initialNotifications } from "./notifications";
import { NotificationsPopover } from "./notifications-popover";

const useNotifications = () => {
  const [notifications, setNotifications] =
    useState<Notification[]>(initialNotifications);
  const unread = useMemo((): number => {
    return notifications.reduce(
      (acc, notification) => acc + (notification.read ? 0 : 1),
      0,
    );
  }, [notifications]);

  const handleRemoveOne = useCallback((notificationId: string): void => {
    setNotifications((prevState) => {
      return prevState.filter(
        (notification) => notification.id !== notificationId,
      );
    });
  }, []);

  const handleMarkAllAsRead = useCallback((): void => {
    setNotifications((prevState) => {
      return prevState.map((notification) => ({
        ...notification,
        read: true,
      }));
    });
  }, []);

  return {
    handleMarkAllAsRead,
    handleRemoveOne,
    notifications,
    unread,
  };
};

export const NotificationsButton: FC = () => {
  const anchorRef = useRef<HTMLButtonElement | null>(null);
  const [openPopover, setOpenPopover] = useState<boolean>(false);
  const { handleRemoveOne, handleMarkAllAsRead, notifications, unread } =
    useNotifications();

  const handlePopoverOpen = useCallback((): void => {
    setOpenPopover(true);
  }, []);

  const handlePopoverClose = useCallback((): void => {
    setOpenPopover(false);
  }, []);

  return (
    <>
      <Tooltip title="Notifications">
        <IconButton ref={anchorRef} onClick={handlePopoverOpen}>
          <Badge color="error" badgeContent={unread}>
            <SvgIcon>
              <NotificationsNoneIcon />
            </SvgIcon>
          </Badge>
        </IconButton>
      </Tooltip>
      <NotificationsPopover
        anchorEl={anchorRef.current}
        notifications={notifications}
        onClose={handlePopoverClose}
        onMarkAllAsRead={handleMarkAllAsRead}
        onRemoveOne={handleRemoveOne}
        open={openPopover}
      />
    </>
  );
};
