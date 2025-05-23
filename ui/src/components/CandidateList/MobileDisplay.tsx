import { Box } from "@mui/material";
import { MutableRefObject } from "react";

import { CandidatesStack } from "@/components/CandidateList/CandidatesStack";
import { HydrophonesStack } from "@/components/CandidateList/HydrophonesStack";
import { VisualizationsStack } from "@/components/CandidateList/VisualizationsStack";

import { MapWrapper } from "../layouts/HalfMapLayout/MapWrapper";
import { MobileContainer } from "../layouts/HalfMapLayout/MobileContainer";

type Props = {
  menuTab: number;
  tabValue: number;
  masterPlayerTimeRef: MutableRefObject<number>;
};

export function MobileDisplay({
  menuTab,
  tabValue,
  masterPlayerTimeRef,
}: Props) {
  return (
    <Box
      className={"mobile-display"}
      sx={{
        display: "flex",
        flexDirection: "column",
        flex: 1,
        overflowY: "auto",
      }}
    >
      {menuTab === 0 && (
        <Box
          className={"tab-content"}
          sx={{
            flex: 1,
            display: "flex",
            flexFlow: "column",
          }}
        >
          {tabValue === 0 && ( // Map
            <>
              <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
            </>
          )}
          {tabValue === 1 && ( // Candidates
            <MobileContainer>
              <CandidatesStack />
            </MobileContainer>
          )}
          {tabValue === 2 && ( // Visualizations
            <MobileContainer>
              <VisualizationsStack />
            </MobileContainer>
          )}
        </Box>
      )}
      {menuTab === 1 && (
        <>
          {tabValue === 0 && ( // Map
            <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
          )}
          {tabValue === 1 && ( // Hydrophones
            <MobileContainer>
              <HydrophonesStack />
            </MobileContainer>
          )}
        </>
      )}
    </Box>
  );
}
