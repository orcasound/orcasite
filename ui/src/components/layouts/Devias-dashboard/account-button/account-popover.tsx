// import SettingsIcon from '@untitled-ui/icons-react/build/esm/Settings04';
import PersonOutlineIcon from "@mui/icons-material/PersonOutline";
// import toast from 'react-hot-toast';
// import CreditCard01Icon from '@untitled-ui/icons-react/build/esm/CreditCard01';
import SettingsIcon from "@mui/icons-material/Settings";
// import PersonOutlineIcon from '@untitled-ui/icons-react/build/esm/User03';
import {
  Box,
  Button,
  Divider,
  ListItemButton,
  ListItemIcon,
  ListItemText,
  Popover,
  SvgIcon,
  Typography,
} from "@mui/material";
import NextLink from "next/link";
import PropTypes from "prop-types";
import type { FC } from "react";
// import { useAuth } from '../../../hooks/use-auth';
// import { paths } from '../../../paths';
// import { Issuer } from '../../../utils/auth';

interface AccountPopoverProps {
  anchorEl: null | Element;
  onClose?: () => void;
  open?: boolean;
}

export const AccountPopover: FC<AccountPopoverProps> = (props) => {
  const { anchorEl, onClose, open, ...other } = props;
  // const router = useRouter();
  // const auth = useAuth();

  // const handleLogout = useCallback(
  //   async (): Promise<void> => {
  //     try {
  //       onClose?.();

  //       switch (auth.issuer) {
  //         case Issuer.Amplify: {
  //           await auth.signOut();
  //           break;
  //         }

  //         case Issuer.Auth0: {
  //           await auth.logout();
  //           break;
  //         }

  //         case Issuer.Firebase: {
  //           await auth.signOut();
  //           break;
  //         }

  //         case Issuer.JWT: {
  //           await auth.signOut();
  //           break;
  //         }

  //         default: {
  //           console.warn('Using an unknown Auth Issuer, did not log out');
  //         }
  //       }

  //       router.push(paths.index);
  //     } catch (err) {
  //       console.error(err);
  //       // toast.error('Something went wrong!');
  //     }
  //   },
  //   [auth, router, onClose]
  // );

  return (
    <Popover
      anchorEl={anchorEl}
      anchorOrigin={{
        horizontal: "center",
        vertical: "bottom",
      }}
      disableScrollLock
      onClose={onClose}
      open={!!open}
      PaperProps={{ sx: { width: 200 } }}
      {...other}
    >
      <Box sx={{ p: 2 }}>
        <Typography variant="body1">Anika Visser</Typography>
        <Typography color="text.secondary" variant="body2">
          demo@devias.io
        </Typography>
      </Box>
      <Divider />
      <Box sx={{ p: 1 }}>
        <ListItemButton
          component={NextLink}
          // href={paths.dashboard.social.profile}
          href="#"
          sx={{
            borderRadius: 1,
            px: 1,
            py: 0.5,
          }}
        >
          <ListItemIcon>
            <SvgIcon fontSize="small">
              <PersonOutlineIcon />
            </SvgIcon>
          </ListItemIcon>
          <ListItemText
            primary={<Typography variant="body1">Profile</Typography>}
          />
        </ListItemButton>
        <ListItemButton
          component={NextLink}
          // href={paths.dashboard.account}
          href="#"
          sx={{
            borderRadius: 1,
            px: 1,
            py: 0.5,
          }}
        >
          <ListItemIcon>
            <SvgIcon fontSize="small">
              <SettingsIcon />
            </SvgIcon>
          </ListItemIcon>
          <ListItemText
            primary={<Typography variant="body1">Settings</Typography>}
          />
        </ListItemButton>
        {/* <ListItemButton
          component={NextLink}
          href={paths.dashboard.index}
          sx={{
            borderRadius: 1,
            px: 1,
            py: 0.5
          }}
        >
          <ListItemIcon>
            <SvgIcon fontSize="small">
              <CreditCard01Icon />
            </SvgIcon>
          </ListItemIcon>
          <ListItemText
            primary={(
              <Typography variant="body1">
                Billing
              </Typography>
            )}
          />
        </ListItemButton> */}
      </Box>
      <Divider sx={{ my: "0 !important" }} />
      <Box
        sx={{
          display: "flex",
          p: 1,
          justifyContent: "center",
        }}
      >
        <Button
          color="inherit"
          // onClick={handleLogout}
          size="small"
        >
          Logout
        </Button>
      </Box>
    </Popover>
  );
};

AccountPopover.propTypes = {
  anchorEl: PropTypes.any,
  onClose: PropTypes.func,
  open: PropTypes.bool,
};
