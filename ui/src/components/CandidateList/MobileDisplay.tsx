import { Box } from "@mui/material";
import { useRouter } from "next/router";
import { MutableRefObject, useEffect, useState } from "react";

import { HydrophonesStack } from "@/components/CandidateList/HydrophonesStack";
import { useLayout } from "@/context/LayoutContext";

import { MapWrapper } from "../layouts/HalfMapLayout/MapWrapper";
import { MobileContainer } from "../layouts/HalfMapLayout/MobileContainer";
import { CandidatesStack } from "./CandidatesStack";
import { MobileTabs } from "./MobileTabs";

type Props = {
  masterPlayerTimeRef: MutableRefObject<number>;
};

export function MobileDisplay({ masterPlayerTimeRef }: Props) {
  // tabValue is the state of the top tabs (Listen Live, Last 7 Days) in <MobileTabs>
  const [tabValue, setTabValue] = useState(0);

  // mobileTab is the state of the bottom tabs in <MobileBottomNav>
  const { mobileTab, setMobileTab } = useLayout();

  const router = useRouter();

  useEffect(() => {
    if (router.route === "/beta/hydrophones") {
      setTabValue(0);
      setMobileTab(1);
    } else if (router.route === "/beta/candidates") {
      setTabValue(1);
      setMobileTab(1);
    } else if (router.route === "/beta") {
      setMobileTab(0);
    }
  }, [router, setTabValue, setMobileTab]);

  return (
    <>
      <Box
        className={"map-tab-content"}
        sx={{
          flex: 1,
          display: mobileTab === 0 ? "flex" : "none",
          flexFlow: "column",
        }}
      >
        <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
      </Box>
      {mobileTab === 1 && (
        <>
          <MobileTabs tabValue={tabValue} setTabValue={setTabValue} />
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
