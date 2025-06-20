import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import React, { MutableRefObject } from "react";

import PlayBar from "@/components/PlayBar/PlayBar";
import { useLayout } from "@/context/LayoutContext";

export default function Footer({
  masterPlayerTimeRef,
}: {
  masterPlayerTimeRef: MutableRefObject<number>;
}) {
  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const { mobileMenuHeight } = useLayout();

  return (
    <Stack
      direction="column"
      className={"bottom-controls-stack"}
      sx={{
        position: "absolute",
        bottom: smDown ? mobileMenuHeight : 0,
        // top: "8rem",
        zIndex: (theme) => theme.zIndex.drawer + 1,
        width: "100%",
        margin: "auto",
        justifyContent: "flex-end",
        alignItems: "center",
        // backgroundColor: "rgba(0,0,0,.15)",
        // backdropFilter: "blur(10px)",
        // WebkitBackdropFilter: "blur(10px)",

        // maskImage: "linear-gradient(to bottom, black 70%, transparent 100%)",
        // WebkitMaskImage:
        //   "linear-gradient(to bottom, black 70%, transparent 100%)",
      }}
    >
      <Box
        className="playbar-container"
        sx={{
          width: smDown ? "calc(100% - .5rem)" : "46%",
          mb: smDown ? "2px" : "2rem",
        }}
      >
        <PlayBar masterPlayerTimeRef={masterPlayerTimeRef} />
      </Box>
    </Stack>
  );
}
