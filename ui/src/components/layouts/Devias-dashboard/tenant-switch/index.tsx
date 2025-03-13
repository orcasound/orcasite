import { Box, IconButton, Stack, SvgIcon, Typography } from "@mui/material";
import { SxProps } from "@mui/system";
import ChevronDownIcon from "icons-react/build/esm/ChevronDown";
import PropTypes from "prop-types";
import type { FC } from "react";
import { useCallback, useRef, useState } from "react";

import { TenantPopover } from "./tenant-popover";

const tenants: string[] = ["Devias", "Acme Corp"];

interface TenantSwitchProps {
  sx?: SxProps;
}

export const TenantSwitch: FC<TenantSwitchProps> = (props) => {
  const anchorRef = useRef<HTMLButtonElement | null>(null);
  const [openPopover, setOpenPopover] = useState<boolean>(false);

  const handlePopoverOpen = useCallback(() => {
    setOpenPopover(true);
  }, []);

  const handlePopoverClose = useCallback(() => {
    setOpenPopover(false);
  }, []);

  const handleTenantChange = useCallback((_tenant: string) => {
    setOpenPopover(false);
  }, []);

  return (
    <>
      <Stack alignItems="center" direction="row" spacing={2} {...props}>
        <Box sx={{ flexGrow: 1 }}>
          <Typography color="inherit" variant="h6">
            Devias
          </Typography>
          <Typography color="neutral.400" variant="body2">
            Production
          </Typography>
        </Box>
        <IconButton onClick={handlePopoverOpen} ref={anchorRef}>
          <SvgIcon sx={{ fontSize: 16 }}>
            <ChevronDownIcon />
          </SvgIcon>
        </IconButton>
      </Stack>
      <TenantPopover
        anchorEl={anchorRef.current}
        onChange={handleTenantChange}
        onClose={handlePopoverClose}
        open={openPopover}
        tenants={tenants}
      />
    </>
  );
};

TenantSwitch.propTypes = {
  sx: PropTypes.object,
};
