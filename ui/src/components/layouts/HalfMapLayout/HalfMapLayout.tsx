import { Box, Theme, useMediaQuery } from "@mui/material";
import { AnimatePresence, motion } from "framer-motion";
import { useRouter } from "next/router";
import {
  ReactElement,
  ReactNode,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import DesktopTabs from "@/components/CandidateList/DesktopTabs";
import { MobileDisplay } from "@/components/CandidateList/MobileDisplay";
import { MobileTabs } from "@/components/CandidateList/MobileTabs";
import Header from "@/components/Header";
import { useData } from "@/context/DataContext";
import { LayoutContext } from "@/context/LayoutContext";
import { useNowPlaying } from "@/context/NowPlayingContext";

import { MasterDataLayout } from "../MasterDataLayout";
import Footer from "./Footer";
import { MapWrapper } from "./MapWrapper";
import { SideList } from "./SideList";

function HalfMapLayout({ children }: { children: ReactNode }) {
  const router = useRouter();
  const pageRoute = useMemo(() => router.route, [router.route]);
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  const {
    nowPlayingCandidate,
    setNowPlayingCandidate,
    nowPlayingFeed,
    setNowPlayingFeed,
    setQueue,
  } = useNowPlaying();
  const { feeds, sortedCandidates, autoPlayOnReady } = useData();

  const masterPlayerTimeRef = useRef(0);

  // menuTab is the state of the mobile <MobileBottomNav>
  const [menuTab, setMenuTab] = useState(0);

  // tabValue is the state of the tabs at the top in <MobileTabs>
  const [tabValue, setTabValue] = useState(0);

  useEffect(() => {
    if (pageRoute === "/beta/hydrophones" || pageRoute === "/beta") {
      setMenuTab(1);
      setTabValue(0);
      if (!nowPlayingFeed) {
        autoPlayOnReady.current = false;
        setNowPlayingFeed(feeds[0]);
        setNowPlayingCandidate(null);
        console.log(
          "just ran this condition and autoPlayOnReady is: " +
            autoPlayOnReady.current,
        );
      }
    } else if (pageRoute === "/beta/candidates") {
      setMenuTab(0);
      setTabValue(1);
      if (!nowPlayingCandidate) {
        autoPlayOnReady.current = false;
        setNowPlayingCandidate(sortedCandidates[0]);
        setNowPlayingFeed(null);
      }
      if (setQueue) setQueue(sortedCandidates);
    } else if (pageRoute === "/beta/visualizations") {
      setMenuTab(0);
      setTabValue(2);
      if (!nowPlayingCandidate && !nowPlayingFeed) {
        autoPlayOnReady.current = false;
        setNowPlayingCandidate(sortedCandidates[0]);
        setNowPlayingFeed(null);
      }
    }
  }, [
    pageRoute,
    setMenuTab,
    setTabValue,
    nowPlayingFeed,
    nowPlayingCandidate,
    feeds,
    autoPlayOnReady.current,
    setNowPlayingCandidate,
    setNowPlayingFeed,
    sortedCandidates,
  ]);

  const showChildren = useMemo(() => {
    return (
      router.query.candidateId !== undefined ||
      router.query.feedSlug !== undefined
    );
  }, [router.query]);

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
          paddingBottom: smDown ? "155px" : "86px",
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
          {!mdDown && (
            <SideList>
              <AnimatePresence mode="wait">
                {showChildren ? (
                  <motion.div
                    key="children"
                    initial={{ y: 100, opacity: 0 }}
                    animate={{ y: 0, opacity: 1 }}
                    exit={{ y: 100, opacity: 0 }}
                    transition={{ duration: 0.4, ease: "easeOut" }}
                    style={{ height: "100%" }}
                  >
                    {children}
                  </motion.div>
                ) : (
                  <DesktopTabs />
                )}
              </AnimatePresence>
            </SideList>
          )}
          {!mdDown && <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />}

          {/* // mobile view */}
          {mdDown && (
            <>
              {showChildren ? (
                children
              ) : (
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
            </>
          )}
          {/* // candidate or feed detail view */}
          {/* {(router.query.candidateId !== undefined ||
            router.query.feed !== undefined) &&
            children} */}
        </Box>

        <Footer
          masterPlayerTimeRef={masterPlayerTimeRef}
          menuTab={menuTab}
          setMenuTab={setMenuTab}
          setTabValue={setTabValue}
        />
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
