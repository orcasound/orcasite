import {
  Avatar,
  Box,
  Link,
  List,
  ListItem,
  ListItemAvatar,
  ListItemText,
  Popover,
  Typography,
} from "@mui/material";
import { formatDistanceStrict } from "date-fns";
import PropTypes from "prop-types";
import type { FC } from "react";
// import { Presence } from '../../../components/presence';
// import { customLocale } from '../../../utils/date-locale';

interface Contact {
  id: string;
  avatar: string;
  isActive: boolean;
  lastActivity?: number;
  name: string;
}

interface ContactsPopoverProps {
  anchorEl: null | Element;
  contacts?: Contact[];
  onClose?: () => void;
  open?: boolean;
}

export const ContactsPopover: FC<ContactsPopoverProps> = (props) => {
  const { anchorEl, contacts = [], onClose, open = false, ...other } = props;

  return (
    <Popover
      anchorEl={anchorEl}
      anchorOrigin={{
        horizontal: "center",
        vertical: "bottom",
      }}
      disableScrollLock
      onClose={onClose}
      open={open}
      PaperProps={{ sx: { width: 320 } }}
      {...other}
    >
      <Box sx={{ p: 2 }}>
        <Typography variant="h6">Contacts</Typography>
      </Box>
      <Box sx={{ p: 2 }}>
        <List disablePadding>
          {contacts.map((contact) => {
            // const showOnline = contact.isActive;
            const lastActivity =
              !contact.isActive && contact.lastActivity
                ? formatDistanceStrict(contact.lastActivity, new Date(), {
                    addSuffix: true,
                    // locale: customLocale
                  })
                : undefined;

            return (
              <ListItem disableGutters key={contact.id}>
                <ListItemAvatar>
                  <Avatar src={contact.avatar} sx={{ cursor: "pointer" }} />
                </ListItemAvatar>
                <ListItemText
                  disableTypography
                  primary={
                    <Link
                      color="text.primary"
                      noWrap
                      sx={{ cursor: "pointer" }}
                      underline="none"
                      variant="subtitle2"
                    >
                      {contact.name}
                    </Link>
                  }
                />
                {/* {showOnline && (
                  <Presence
                    size="small"
                    status="online"
                  />
                )} */}
                {lastActivity && (
                  <Typography color="text.secondary" noWrap variant="caption">
                    {lastActivity}
                  </Typography>
                )}
              </ListItem>
            );
          })}
        </List>
      </Box>
    </Popover>
  );
};

ContactsPopover.propTypes = {
  anchorEl: PropTypes.any,
  contacts: PropTypes.array,
  onClose: PropTypes.func,
  open: PropTypes.bool,
};
