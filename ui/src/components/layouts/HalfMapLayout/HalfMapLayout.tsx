import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import { useRouter } from "next/router";
import { ReactElement, ReactNode, useRef, useState } from "react";

import Header from "@/components/Header";
import { LayoutContext } from "@/context/LayoutContext";

import PlayBar from "../../PlayBar/PlayBar";
import { MasterDataLayout } from "../MasterDataLayout";
import { MainPanel } from "./MainPanel";
import { MobileBottomNav } from "./MobileBottomNav";
import { SideList } from "./SideList";

function HalfMapLayout({ children }: { children: ReactNode }) {
  const router = useRouter();
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  console.log("rendering halfmap");

  const masterPlayerTimeRef = useRef(0);

  // menuTabs are the mobile bottom navigation
  const [menuTab, setMenuTab] = useState(0);

  return (
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
        display: "flex",
        flexDirection: "column",
        flexGrow: 1,
      }}
    >
      <Header />
      <Box
        component="main"
        sx={{
          display: "flex",
          flexFlow: mdDown ? "column" : "row",
          flex: 1,
          overflow: "hidden",
          position: "relative",
        }}
      >
        {!mdDown && <SideList />}
        {router.query.candidateId === undefined ? (
          <MainPanel
            menuTab={menuTab}
            masterPlayerTimeRef={masterPlayerTimeRef}
          />
        ) : (
          children
        )}

        <Stack
          direction="column"
          sx={{
            position: "fixed",
            bottom: 0,
            zIndex: (theme) => theme.zIndex.drawer + 1,
            width: "100%",
          }}
        >
          <PlayBar masterPlayerTimeRef={masterPlayerTimeRef} />
          {mdDown && (
            <MobileBottomNav menuTab={menuTab} setMenuTab={setMenuTab} />
          )}
        </Stack>
      </Box>
    </Box>
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
