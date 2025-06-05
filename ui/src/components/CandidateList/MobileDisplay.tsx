import { Box } from "@mui/material";
import { MutableRefObject, useState } from "react";

import { HydrophonesStack } from "@/components/CandidateList/HydrophonesStack";

import { MapWrapper } from "../layouts/HalfMapLayout/MapWrapper";
import { MobileContainer } from "../layouts/HalfMapLayout/MobileContainer";
import { CandidatesStack } from "./CandidatesStack";
import { MobileTabs } from "./MobileTabs";

type Props = {
  menuTab: number;
  masterPlayerTimeRef: MutableRefObject<number>;
};

export function MobileDisplay({ menuTab, masterPlayerTimeRef }: Props) {
  // tabValue is the state of the tabs at the top in <MobileTabs>
  const [tabValue, setTabValue] = useState(0);

  return (
    <>
      {menuTab === 0 && (
        <Box
          className={"tab-content"}
          sx={{
            flex: 1,
            display: "flex",
            flexFlow: "column",
          }}
        >
          <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
        </Box>
      )}
      {menuTab === 1 && (
        <>
          <MobileTabs
            menuTab={menuTab}
            tabValue={tabValue}
            setTabValue={setTabValue}
          />
          {tabValue === 0 && ( // Map
            <MobileContainer>
              <HydrophonesStack />
            </MobileContainer>
          )}
          {tabValue === 1 && ( // Hydrophones
            <MobileContainer>
              <CandidatesStack />
            </MobileContainer>
          )}
        </>
      )}
    </>
  );
}
