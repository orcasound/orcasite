import { Box } from "@mui/material";
import { useRouter } from "next/router";
import { MutableRefObject, SetStateAction, useEffect, useState } from "react";

import { HydrophonesStack } from "@/components/CandidateList/HydrophonesStack";

import { MapWrapper } from "../layouts/HalfMapLayout/MapWrapper";
import { MobileContainer } from "../layouts/HalfMapLayout/MobileContainer";
import { CandidatesStack } from "./CandidatesStack";
import { MobileTabs } from "./MobileTabs";

type Props = {
  menuTab: number;
  setMenuTab: React.Dispatch<SetStateAction<number>>;
  masterPlayerTimeRef: MutableRefObject<number>;
};

export function MobileDisplay({
  menuTab,
  setMenuTab,
  masterPlayerTimeRef,
}: Props) {
  // tabValue is the state of the tabs at the top in <MobileTabs>
  const [tabValue, setTabValue] = useState(0);
  const router = useRouter();
  useEffect(() => {
    if (router.route === "/beta/hydrophones") {
      setTabValue(0);
      setMenuTab(1);
    } else if (router.route === "/beta/candidates") {
      setTabValue(1);
      setMenuTab(1);
    } else if (router.route === "/beta") {
      setMenuTab(0);
    }
  }, [router, setTabValue, setMenuTab]);

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
