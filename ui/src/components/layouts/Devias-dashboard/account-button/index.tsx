// import User01Icon from '@untitled-ui/icons-react/build/esm/User01';
import FaceIcon from "@mui/icons-material/Face";
import { Avatar, Box, ButtonBase, SvgIcon } from "@mui/material";
import type { FC } from "react";
import { useCallback, useRef, useState } from "react";

// import { useMockedUser } from '../../../hooks/use-mocked-user';
import { AccountPopover } from "./account-popover";

export const AccountButton: FC = () => {
  // const user = useMockedUser();
  // const user = {
  //   id: "5e86809283e28b96d2d38537",
  //   avatar: "/assets/avatars/avatar-anika-visser.png",
  //   name: "Anika Visser",
  //   email: "anika.visser@devias.io",
  // };
  const anchorRef = useRef<HTMLButtonElement | null>(null);
  const [openPopover, setOpenPopover] = useState<boolean>(false);

  const handlePopoverOpen = useCallback((): void => {
    setOpenPopover(true);
  }, []);

  const handlePopoverClose = useCallback((): void => {
    setOpenPopover(false);
  }, []);

  return (
    <>
      <Box
        component={ButtonBase}
        onClick={handlePopoverOpen}
        ref={anchorRef}
        sx={{
          alignItems: "center",
          display: "flex",
          borderWidth: 2,
          borderStyle: "solid",
          borderColor: "divider",
          height: 40,
          width: 40,
          borderRadius: "50%",
        }}
      >
        <Avatar
          sx={{
            height: 32,
            width: 32,
          }}
          // src={user.avatar}
        >
          <SvgIcon>
            <FaceIcon />
          </SvgIcon>
        </Avatar>
      </Box>
      <AccountPopover
        anchorEl={anchorRef.current}
        onClose={handlePopoverClose}
        open={openPopover}
      />
    </>
  );
};
