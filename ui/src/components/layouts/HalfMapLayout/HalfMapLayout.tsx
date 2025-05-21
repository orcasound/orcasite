import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import { useRouter } from "next/router";
import { ReactElement, ReactNode, useRef, useState } from "react";

import { MobileDisplay } from "@/components/CandidateList/MobileDisplay";
import { MobileTabs } from "@/components/CandidateList/MobileTabs";
import Header from "@/components/Header";
import { LayoutContext } from "@/context/LayoutContext";

import PlayBar from "../../PlayBar/PlayBar";
import { MasterDataLayout } from "../MasterDataLayout";
import { MapWrapper } from "./MapWrapper";
import { MobileBottomNav } from "./MobileBottomNav";
import { SideList } from "./SideList";

function HalfMapLayout({ children }: { children: ReactNode }) {
  const router = useRouter();
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  console.log("rendering halfmap");

  const masterPlayerTimeRef = useRef(0);

  // menuTab is the state of the mobile <MobileBottomNav>
  const [menuTab, setMenuTab] = useState(0);

  // tabValue is the state of the tabs at the top in <MobileTabs>
  const [tabValue, setTabValue] = useState(0);

  return (
    <>
      <Header />
      <Box
        sx={{
          // use `dvh` for dynamic viewport height to handle mobile browser weirdness
          // but fallback to `vh` for browsers that don't support `dvh`
          // `&` is a workaround because sx prop can't have identical keys
          // "&": {
          //   height: "100dvh",
          // },
          height: "100vh",
          paddingBottom: smDown ? "155px" : "80px",
          paddingTop: "60px", // added this due to making header position: fixed
          display: "flex",
          flexDirection: "column",
          flexGrow: 1,
        }}
      >
        <Box
          component="main"
          sx={{
            display: "flex",
            flexFlow: mdDown ? "column" : "row",
            flex: 1,
            position: "relative",
            overflow: "hidden",
          }}
        >
          {/* // desktop view */}
          {!mdDown && <SideList />}
          {!mdDown && router.query.candidateId === undefined && (
            <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />
          )}

          {/* // mobile view */}
          {mdDown && router.query.candidateId === undefined && (
            <>
              <MobileTabs
                menuTab={menuTab}
                tabValue={tabValue}
                setTabValue={setTabValue}
              />
              <MobileDisplay
                menuTab={menuTab}
                tabValue={tabValue}
                masterPlayerTimeRef={masterPlayerTimeRef}
              />
            </>
          )}
          {/* // candidate or feed detail view */}
          {(router.query.candidateId !== undefined ||
            router.query.feed !== undefined) &&
            children}

          <Stack
            direction="column"
            className={"bottom-controls-stack"}
            sx={{
              position: "fixed",
              bottom: 0,
              zIndex: (theme) => theme.zIndex.drawer + 1,
              width: "100%",
            }}
          >
            <PlayBar masterPlayerTimeRef={masterPlayerTimeRef} />
            {mdDown && (
              <MobileBottomNav
                menuTab={menuTab}
                setMenuTab={setMenuTab}
                setTabValue={setTabValue}
              />
            )}
          </Stack>
        </Box>
      </Box>
    </>
  );
}

export function getHalfMapLayout(page: ReactElement) {
  return (
    <MasterDataLayout>
      <LayoutContext.Provider value="halfMap">
        <HalfMapLayout>{page}</HalfMapLayout>
      </LayoutContext.Provider>
    </MasterDataLayout>
  );
}
