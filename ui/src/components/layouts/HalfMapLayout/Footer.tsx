import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import React, { MutableRefObject, SetStateAction, useState } from "react";

import PlayBar from "@/components/PlayBar/PlayBar";
import darkTheme from "@/styles/darkTheme";

import { MobileBottomNav } from "./MobileBottomNav";
import PlayerDetail from "./PlayerDetail";

export default function Footer({
  masterPlayerTimeRef,
  menuTab,
  setMenuTab,
}: {
  masterPlayerTimeRef: MutableRefObject<number>;
  menuTab: number;
  setMenuTab: React.Dispatch<SetStateAction<number>>;
}) {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  // const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const [playbarExpanded, setPlaybarExpanded] = useState(false);

  return (
    <Stack
      direction="column"
      className={"bottom-controls-stack"}
      sx={{
        position: "static",
        bottom: 0,
        zIndex: (theme) => theme.zIndex.drawer + 1,
        width: "100%",
        backgroundColor: darkTheme.palette.background.default,
        height: playbarExpanded
          ? "calc(100vh - 64px)"
          : mdDown
            ? "141px"
            : "86px",
        transition: "height .66s ease",
      }}
    >
      <PlayBar
        masterPlayerTimeRef={masterPlayerTimeRef}
        playbarExpanded={playbarExpanded}
        setPlaybarExpanded={setPlaybarExpanded}
      />
      {playbarExpanded && (
        <Box sx={{ overflow: "scroll", px: "24px" }}>
          <PlayerDetail />
        </Box>
      )}
      {mdDown && <MobileBottomNav menuTab={menuTab} setMenuTab={setMenuTab} />}
    </Stack>
  );
}
