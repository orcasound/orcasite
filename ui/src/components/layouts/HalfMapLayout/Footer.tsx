import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import React, { MutableRefObject, SetStateAction, useState } from "react";

import PlayBar from "@/components/PlayBar/PlayBar";
import { useLayout } from "@/context/LayoutContext";

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
  const { headerHeight } = useLayout();

  return (
    <Stack
      direction="column"
      className={"bottom-controls-stack"}
      sx={{
        position: "static",
        bottom: 0,
        zIndex: (theme) => theme.zIndex.drawer + 1,
        width: "100%",
        justifyContent: "flex-end",
      }}
    >
      <PlayBar
        masterPlayerTimeRef={masterPlayerTimeRef}
        playbarExpanded={playbarExpanded}
        setPlaybarExpanded={setPlaybarExpanded}
      />
      <Box
        className="now-playing-drawer"
        sx={{
          px: "24px",
          flex: 1,
          overflowY: "auto",
          position: "absolute",
          bottom: mdDown ? "69px" : 0,
          width: "100%",
          height: playbarExpanded
            ? mdDown
              ? "calc(100vh - 69px)"
              : `calc(100vh - ${headerHeight})`
            : 0,
          backgroundColor: "background.default",
          zIndex: (theme) => theme.zIndex.drawer + 1,
          transition: "height .66s ease",
        }}
      >
        <PlayerDetail setPlaybarExpanded={setPlaybarExpanded} />
      </Box>
      {mdDown && <MobileBottomNav menuTab={menuTab} setMenuTab={setMenuTab} />}
    </Stack>
  );
}
