import { IconButton, SvgIcon, Tooltip } from "@mui/material";
import { subHours, subMinutes } from "date-fns";
import Users03Icon from "icons-react/build/esm/Users03";
import type { FC } from "react";
import { useCallback, useRef, useState } from "react";

import { ContactsPopover } from "./contacts-popover";

const now = new Date();

interface Contact {
  id: string;
  avatar: string;
  isActive: boolean;
  lastActivity?: number;
  name: string;
}

const useContacts = (): Contact[] => {
  return [
    {
      id: "5e8891ab188cd2855e6029b7",
      avatar: "/assets/avatars/avatar-alcides-antonio.png",
      isActive: true,
      lastActivity: now.getTime(),
      name: "Alcides Antonio",
    },
    {
      id: "5e887a62195cc5aef7e8ca5d",
      avatar: "/assets/avatars/avatar-marcus-finn.png",
      isActive: false,
      lastActivity: subHours(now, 2).getTime(),
      name: "Marcus Finn",
    },
    {
      id: "5e887ac47eed253091be10cb",
      avatar: "/assets/avatars/avatar-carson-darrin.png",
      isActive: false,
      lastActivity: subMinutes(now, 15).getTime(),
      name: "Carson Darrin",
    },
    {
      id: "5e887b209c28ac3dd97f6db5",
      avatar: "/assets/avatars/avatar-fran-perez.png",
      isActive: true,
      lastActivity: now.getTime(),
      name: "Fran Perez",
    },
    {
      id: "5e887b7602bdbc4dbb234b27",
      avatar: "/assets/avatars/avatar-jie-yan-song.png",
      isActive: true,
      lastActivity: now.getTime(),
      name: "Jie Yan Song",
    },
  ];
};

export const ContactsButton: FC = () => {
  const anchorRef = useRef<HTMLButtonElement | null>(null);
  const [openPopover, setOpenPopover] = useState<boolean>(false);
  const contacts = useContacts();

  const handlePopoverOpen = useCallback((): void => {
    setOpenPopover(true);
  }, []);

  const handlePopoverClose = useCallback((): void => {
    setOpenPopover(false);
  }, []);

  return (
    <>
      <Tooltip title="Contacts">
        <IconButton onClick={handlePopoverOpen} ref={anchorRef}>
          <SvgIcon>
            <Users03Icon />
          </SvgIcon>
        </IconButton>
      </Tooltip>
      <ContactsPopover
        anchorEl={anchorRef.current}
        contacts={contacts}
        onClose={handlePopoverClose}
        open={openPopover}
      />
    </>
  );
};
