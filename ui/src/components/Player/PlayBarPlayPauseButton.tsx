import { ErrorOutline, Pause, PlayArrow } from "@mui/icons-material";
import { Box, CircularProgress, Tooltip } from "@mui/material";

import { type PlayerStatus } from "./Player";

export default function PlayBarPlayPauseButton({
  playerStatus,
  disabled,
  onClick,
}: {
  playerStatus: PlayerStatus;
  disabled: boolean;
  onClick: () => void;
}) {
  return (
    <Box
      onClick={() => !disabled && onClick()}
      sx={{ display: "flex", justifyContent: "center" }}
    >
      {playerStatus === "error" ? (
        <Tooltip title="Failed to load" placement="right">
          <ErrorOutline className="icon" fontSize="large" />
        </Tooltip>
      ) : playerStatus === "loading" ? (
        <CircularProgress sx={{ color: "base.contrastText" }} />
      ) : playerStatus === "playing" ? (
        <Pause className="icon" fontSize="large" />
      ) : (
        <PlayArrow className="icon" fontSize="large" />
      )}
    </Box>
  );
}
