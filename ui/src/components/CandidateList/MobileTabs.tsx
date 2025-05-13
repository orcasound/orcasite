import { Box } from "@mui/material";
import { MutableRefObject, useState } from "react";

import { CandidatesStack } from "@/components/CandidateList/CandidatesStack";
import { HydrophonesStack } from "@/components/CandidateList/HydrophonesStack";
import { VisualizationsStack } from "@/components/CandidateList/VisualizationsStack";

import { MapWrapper } from "../layouts/HalfMapLayout/MapWrapper";
import { MobileContainer } from "../layouts/HalfMapLayout/MobileContainer";

type Props = {
  menuTab: number;
  masterPlayerTimeRef: MutableRefObject<number>;
};

export function MobileTabs({ menuTab, masterPlayerTimeRef }: Props) {
  const [tabValue, setTabValue] = useState(0);

  const handleChange = (event: React.MouseEvent<HTMLDivElement>) => {
    setTabValue(Number(event.currentTarget.id));
  };

  const recordingsTabs = ["Map", "Candidates", "Visualizations"];
  const listenLiveTabs = ["Map", "Hydrophones"];

  const makeTabs = (array: string[]) => {
    return (
      <Box
        sx={{
          display: "flex",
          justifyContent: "space-around",
          alignItems: "center",
          gap: "1rem",
          borderBottom: "1px solid rgba(255,255,255,.3)",
        }}
      >
        {array.map((tab: string, index: number) => {
          return (
            <Box
              id={index.toString()}
              key={tab}
              onClick={handleChange}
              sx={{
                borderBottom: index === tabValue ? "1.5px solid #fff" : "none",
                flex: 1,
                textAlign: "center",
                py: 1,
                color:
                  index === tabValue
                    ? "rgba(255,255,255,1)"
                    : "rgba(255,255,255,.8)",
              }}
            >
              {tab}
            </Box>
          );
        })}
      </Box>
    );
  };
  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        flex: 1,
        height: "100%",
        // overflowY: "auto",
      }}
    >
      {menuTab === 0 && (
        <Box
          sx={{
            height: "100%",
          }}
        >
          {makeTabs(recordingsTabs)}
          {tabValue === 0 && (
            <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
          )}
          {tabValue === 1 && (
            <MobileContainer>
              <CandidatesStack />
            </MobileContainer>
          )}
          {tabValue === 2 && (
            <MobileContainer>
              <VisualizationsStack />
            </MobileContainer>
          )}
        </Box>
      )}
      {menuTab === 1 && (
        <>
          {makeTabs(listenLiveTabs)}
          {tabValue === 0 && (
            <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
          )}
          {tabValue === 1 && <HydrophonesStack />}
        </>
      )}
    </Box>
  );
}
