import { Logout, Person, Settings } from "@mui/icons-material";
import {
  Box,
  Button,
  IconButton,
  ListItemIcon,
  Menu,
  MenuItem,
  SxProps,
  Theme,
  Typography,
} from "@mui/material";
import router from "next/router";
import { useState } from "react";

import Link from "@/components/Link";
import { useAuth } from "@/hooks/useAuth";

export default function AccountMenu({ sx }: { sx?: SxProps<Theme> }) {
  const { user, signOut } = useAuth();
  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  const open = Boolean(anchorEl);

  const handleClick = (event: React.MouseEvent<HTMLElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  const handleSignOut = () => {
    handleClose();
    signOut({});
    router.push("/");
  };

  if (!user) {
    return (
      <Link href="/join" title="Join Orcasound" sx={sx}>
        <Button
          variant="contained"
          startIcon={<Person />}
          color="primary"
          sx={{
            borderRadius: 8,
          }}
          // TODO(@paulcretu): Add analytics event
          // onClick={() => analytics.nav.joinClicked()}
        >
          Join
        </Button>
      </Link>
    );
  }

  return (
    <>
      <IconButton
        onClick={handleClick}
        size="small"
        sx={{
          marginLeft: "auto",
          color: "white",
          ...sx,
        }}
        aria-controls="account-menu"
        aria-expanded={open}
      >
        <Person />
      </IconButton>
      <Menu
        id="account-menu"
        anchorEl={anchorEl}
        open={open}
        onClose={handleClose}
      >
        <Box sx={{ px: 2, py: 1 }}>
          <Typography variant="subtitle1" noWrap>
            {[user.firstName, user.lastName].filter(Boolean).join(" ") ??
              user.username ??
              user.email}
          </Typography>
          <Typography variant="body2" color="text.secondary" noWrap>
            {user.email}
          </Typography>
        </Box>
        <MenuItem component={Link} href="/settings">
          <ListItemIcon>
            <Settings fontSize="small" />
          </ListItemIcon>
          Settings
        </MenuItem>
        <MenuItem onClick={handleSignOut}>
          <ListItemIcon>
            <Logout fontSize="small" />
          </ListItemIcon>
          Sign out
        </MenuItem>
      </Menu>
    </>
  );
}
