import { Popover, Typography } from "@mui/material";
import { ReactNode, useState } from "react";

export function TitlePopover({
  children,
  title,
}: {
  children: ReactNode;
  title: string;
}) {
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);

  const handleClick = (event: React.MouseEvent<HTMLElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  const open = Boolean(anchorEl);

  return (
    <>
      <div onClick={handleClick}>{children}</div>
      <Popover
        open={open}
        anchorEl={anchorEl}
        onClose={handleClose}
        anchorOrigin={{
          vertical: "top",
          horizontal: "center",
        }}
        transformOrigin={{
          vertical: "bottom",
          horizontal: "center",
        }}
      >
        <Typography sx={{ p: 2 }}>{title}</Typography>
      </Popover>
    </>
  );
}
