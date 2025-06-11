import { GraphicEq } from "@mui/icons-material";
import { Box, Fab, Theme, useMediaQuery } from "@mui/material";

export default function DetectionButton() {
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
        whiteSpace: "nowrap",
        px: "12px",

        // style to look like outlined button
        backgroundColor: "white",
        color: "background.default",
        borderColor: "primary.main",
        borderStyle: "solid",
        borderWidth: "2px",
        "&:hover": {
          backgroundColor: "primary.main",
          // color: "white",
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
        <GraphicEq sx={{ mr: smDown ? 0 : 1, color: "background.default" }} />
        {!smDown && "Report sound"}
      </Box>
    </Fab>
  );
}
