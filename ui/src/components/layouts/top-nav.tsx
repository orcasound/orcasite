// import MenuIcon from '@untitled-ui/icons-react/build/esm/Menu01';
import MenuIcon from "@mui/icons-material/Menu";
import type { Theme } from "@mui/material";
import { Box, IconButton, Stack, SvgIcon, useMediaQuery } from "@mui/material";
import { alpha } from "@mui/material/styles";
import PropTypes from "prop-types";
import type { FC } from "react";

import { AccountButton } from "./Devias-dashboard/account-button";
import { ContactsButton } from "./Devias-dashboard/contacts-button";
// import { LanguageSwitch } from './Devias-dashboard/language-switch';
import { NotificationsButton } from "./Devias-dashboard/notifications-button";
import { SearchButton } from "./Devias-dashboard/search-button";

const TOP_NAV_HEIGHT: number = 64;
const SIDE_NAV_WIDTH: number = 280;

interface TopNavProps {
  onMobileNavOpen?: () => void;
}

export const TopNav: FC<TopNavProps> = (props) => {
  const { onMobileNavOpen, ...other } = props;
  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  return (
    <Box
      component="header"
      sx={{
        backdropFilter: "blur(6px)",
        backgroundColor: (theme) =>
          alpha(theme.palette.background.default, 0.8),
        position: "sticky",
        left: {
          lg: `${SIDE_NAV_WIDTH}px`,
        },
        top: 0,
        width: {
          lg: `calc(100% - ${SIDE_NAV_WIDTH}px)`,
        },
        zIndex: (theme) => theme.zIndex.appBar,
      }}
      {...other}
    >
      <Stack
        alignItems="center"
        direction="row"
        justifyContent="space-between"
        spacing={2}
        sx={{
          minHeight: TOP_NAV_HEIGHT,
          px: 2,
        }}
      >
        <Stack alignItems="center" direction="row" spacing={2}>
          {!lgUp && (
            <IconButton onClick={onMobileNavOpen}>
              <SvgIcon>
                <MenuIcon />
              </SvgIcon>
            </IconButton>
          )}
          <SearchButton />
        </Stack>
        <Stack alignItems="center" direction="row" spacing={2}>
          {/* <LanguageSwitch /> */}
          <NotificationsButton />
          <ContactsButton />
          <AccountButton />
        </Stack>
      </Stack>
    </Box>
  );
};

TopNav.propTypes = {
  onMobileNavOpen: PropTypes.func,
};
