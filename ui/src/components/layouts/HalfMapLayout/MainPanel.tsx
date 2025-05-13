import { Box, Theme, useMediaQuery } from "@mui/material";
import { MutableRefObject } from "react";

import { MobileTabs } from "@/components/CandidateList/MobileTabs";

import { MapWrapper } from "./MapWrapper";

type Props = {
  menuTab: number;
  masterPlayerTimeRef: MutableRefObject<number>;
};

export function MainPanel({ menuTab, masterPlayerTimeRef }: Props) {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));

  return (
    <Box
      className="main-panel"
      sx={{
        flex: 1,
        position: "relative",
        display: "flex",
      }}
    >
      {mdDown ? (
        <MobileTabs
          menuTab={menuTab}
          masterPlayerTimeRef={masterPlayerTimeRef}
        />
      ) : (
        <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
      )}
    </Box>
  );
}
