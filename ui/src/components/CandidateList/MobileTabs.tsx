import { Box } from "@mui/material";
import { Dispatch, SetStateAction } from "react";

type Props = {
  menuTab: number;
  tabValue: number;
  setTabValue: Dispatch<SetStateAction<number>>;
  // masterPlayerTimeRef: MutableRefObject<number>;
};

export function MobileTabs({ menuTab, tabValue, setTabValue }: Props) {
  const handleChange = (event: React.MouseEvent<HTMLDivElement>) => {
    setTabValue(Number(event.currentTarget.id));
  };

  const recordingsTabs = ["Map", "Candidates", "Visualizations"];
  const listenLiveTabs = ["Map", "Hydrophones"];

  const makeTabs = (array: string[]) => {
    return (
      <Box
        className={"mobile-tabs"}
        sx={{
          display: "flex",
          justifyContent: "space-around",
          alignItems: "center",
          gap: "1rem",
          borderBottom: "1px solid rgba(255,255,255,.3)",
          height: "36px",
        }}
      >
        {array.map((tab: string, index: number) => {
          return (
            <Box
              id={index.toString()}
              className={"mobile-tab"}
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

  return menuTab == 0 ? makeTabs(recordingsTabs) : makeTabs(listenLiveTabs);

  //   return (
  //     <>
  //     <Box
  //       sx={{
  //         display: "flex",
  //         flexDirection: "column",
  //         flex: 1,
  //         height: "100%",
  //         // overflowY: "auto",
  //       }}
  //     >
  //       {menuTab === 0 && (
  //         <Box
  //           sx={{
  //             height: "100%",
  //             display: "flex",
  //             flexDirection: "column"
  //           }}
  //         >
  //           {/* {makeTabs(recordingsTabs)} */}
  //           {tabValue === 0 && (
  //             <>
  //             <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
  //             </>
  //           )}
  //           {tabValue === 1 && (
  //             <MobileContainer>
  //               <CandidatesStack />
  //             </MobileContainer>
  //           )}
  //           {tabValue === 2 && (
  //             <MobileContainer>
  //               <VisualizationsStack />
  //             </MobileContainer>
  //           )}
  //         </Box>
  //       )}
  //       {menuTab === 1 && (
  //         <>
  //           {/* {makeTabs(listenLiveTabs)} */}
  //           {tabValue === 0 && (
  //             <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
  //           )}
  //           {tabValue === 1 && <HydrophonesStack />}
  //         </>
  //       )}
  //     </Box>
  //     </>
  //   );
}
