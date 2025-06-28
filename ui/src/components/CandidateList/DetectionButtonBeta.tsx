import { GraphicEq } from "@mui/icons-material";
import { Box, Fab, Theme, useMediaQuery } from "@mui/material";

export default function DetectionButton({
  disabled = false,
}: {
  disabled?: boolean;
}) {
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  return (
    <Fab
      variant="extended"
      size="large"
      color="primary"
      id="detection-button"
      sx={{
        // position: "absolute",
        // bottom: "86px",
        // left: 0,
        // right: 0,
        // margin: "auto",
        mr: smDown ? 0 : "1rem",
        maxWidth: "max-content",
        minWidth: "unset",
        whiteSpace: "nowrap",
        px: "16px",
        boxShadow: "none",
        // style to look like outlined button
        backgroundColor: disabled ? "rgba(255,255,255,.25)" : "primary.main",
        color: "base.main",
        borderRadius: "8px",
        // borderColor: "primary.main",
        // borderStyle: "solid",
        // borderWidth: "2px",
        "&:hover": {
          backgroundColor: disabled ? "rgba(255,255,255,.25)" : "primary.main",
          cursor: disabled ? "default" : "pointer",
        },
      }}
    >
      <Box
        sx={{
          display: "flex",
          justifyContent: "center",
          alignItems: "center",
        }}
      >
        <GraphicEq sx={{ mr: smDown ? 0 : 1, color: "base.main" }} />
        {!smDown && "Report sound"}
      </Box>
    </Fab>
  );
}
