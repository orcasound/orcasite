import { Link } from "@mui/icons-material";
import { IconButton, Popover, Typography } from "@mui/material";
import React, { useRef, useState } from "react";

const CopyToClipboardButton: React.FC<{ text: string }> = ({ text }) => {
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const buttonRef = useRef<HTMLButtonElement | null>(null);
  const timeoutRef = useRef<number | null>(null);

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(text);
      if (buttonRef.current) {
        setAnchorEl(buttonRef.current);
        if (timeoutRef.current) {
          clearTimeout(timeoutRef.current);
        }
        timeoutRef.current = window.setTimeout(() => {
          setAnchorEl(null);
        }, 3000);
      }
    } catch (error) {
      console.error("Failed to copy text: ", error);
    }
  };

  const handleClose = () => {
    setAnchorEl(null);
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
    }
  };

  const open = Boolean(anchorEl);

  return (
    <>
      <IconButton title={text} onClick={handleCopy} ref={buttonRef}>
        <Link />
      </IconButton>
      <Popover
        open={open}
        anchorEl={anchorEl}
        onClose={handleClose}
        anchorOrigin={{
          vertical: "bottom",
          horizontal: "center",
        }}
        transformOrigin={{
          vertical: "top",
          horizontal: "center",
        }}
        disableAutoFocus
        disableEnforceFocus
      >
        <Typography variant="subtitle2" sx={{ p: 2 }}>
          Copied: {text}
        </Typography>
      </Popover>
    </>
  );
};

export default CopyToClipboardButton;
