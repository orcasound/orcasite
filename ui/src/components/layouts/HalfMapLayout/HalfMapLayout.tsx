import {
  Box,
  Container,
  Tab,
  Tabs,
  Theme,
  Typography,
  useMediaQuery,
} from "@mui/material";
import { useRouter } from "next/router";
import { ReactElement, ReactNode, useMemo, useRef, useState } from "react";

import { CandidatesStack } from "@/components/CandidateList/CandidatesStack";
import { HydrophonesStack } from "@/components/CandidateList/HydrophonesStack";
import { MobileDisplay } from "@/components/CandidateList/MobileDisplay";
import { MobileTabs } from "@/components/CandidateList/MobileTabs";
import HeaderNew from "@/components/HeaderNew";
import { LayoutContext } from "@/context/LayoutContext";
import HydrophoneCandidatesPage from "@/pages/beta/[feedSlug]/candidates";
import { getPageContext } from "@/utils/pageContext";

import { MasterDataLayout } from "../MasterDataLayout";
import Footer from "./Footer";
import { MapWrapper } from "./MapWrapper";
import { SideList } from "./SideList";

function HalfMapLayout({ children }: { children: ReactNode }) {
  const router = useRouter();
  const { isFeedDetail } = getPageContext(router);
  console.log("isFeedDetail", isFeedDetail);

  const pageRoute = useMemo(() => router.route, [router.route]);
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  const masterPlayerTimeRef = useRef(0);

  // menuTab is the state of the mobile <MobileBottomNav>
  const [menuTab, setMenuTab] = useState(0);

  // tabValue is the state of the tabs at the top in <MobileTabs>
  const [tabValue, setTabValue] = useState(0);

  const showChildrenLeft = useMemo(() => {
    const showChildren = router.query.candidateId !== undefined;
    console.log("showChildrenLeft", showChildren);
    return showChildren;
  }, [router.query.candidateId]);

  const showChildrenRight = useMemo(() => {
    const showChildren =
      pageRoute === "/beta/[feedSlug]/candidates" ||
      pageRoute === "/beta/[feedSlug]";
    console.log("showChildrenRight", showChildren);
    return showChildren;
  }, [pageRoute]);

  // // if the card is rendered on feed detail, show candidateFeedHref
  // const feedDetailHref = `/beta/[feedSlug]/candidates`;
  // const feedDetailCandidateHref = `/beta/[feedSlug]/[candidateId]`;

  // // if the card is rendered on browse all candidates, show candidateBrowseHref
  // const allCandidatesHref = `/beta/candidates`;
  // const allCandidatesDetailHref = `/beta/candidates/[feedSlug]/[candidateId]`;

  // let isBrowsePage;

  // if (
  //   router.route === feedDetailHref ||
  //   router.route === feedDetailCandidateHref
  // ) {
  //   isBrowsePage = true;
  // } else if (
  //   router.route === allCandidatesHref ||
  //   router.route === allCandidatesDetailHref
  // ) {
  //   isBrowsePage = false;
  // }

  const tabSlugs = ["hydrophones", "candidates", "visualizations"];

  function getTabIndexFromPath(path: string): number {
    const slug = path.replace("/beta/", "");
    if (tabSlugs.includes(slug)) {
      return tabSlugs.indexOf(slug);
    } else {
      return 0;
    }
  }
  const tabIndex = getTabIndexFromPath(router.route);

  function a11yProps(index: number) {
    return {
      id: `simple-tab-${index}`,
      "aria-controls": `simple-tabpanel-${index}`,
    };
  }

  const handleChange = (event: React.SyntheticEvent, newValue: number) => {
    router.push(`/beta/${tabSlugs[newValue]}`, undefined, { shallow: true });
  };

  const tabSx = {
    padding: "7px 16px !important",
    margin: "0 8px !important",
    minWidth: 0,
    minHeight: "unset",
    lineHeight: 1.2,
    borderRadius: "4px",
    "&.Mui-selected": {
      backgroundColor: "rgba(255,255,255,.15)",
      "&:hover": {
        backgroundColor: "rgba(255,255,255,.18)",
      },
    },
    "&:hover": {
      color: "primary.main",
    },
  };

  const tabsSx = {
    minHeight: "unset", // prevent Tabs from enforcing height on children
    ".MuiTabs-indicator": {
      height: "0px",
      bottom: -1,
      backgroundColor: "accent3.main",
    },
  };

  const tabs = (
    <Tabs
      value={tabIndex}
      onChange={handleChange}
      aria-label="navigation tabs"
      centered={mdDown ? true : false}
      sx={tabsSx}
    >
      <Tab
        className="first-tab"
        sx={tabSx}
        label="Hydrophones"
        {...a11yProps(0)}
      />
      <Tab sx={tabSx} label="Explore" {...a11yProps(1)} />
      <Tab sx={tabSx} label="Take Action" {...a11yProps(2)} />
    </Tabs>
  );

  return (
    <>
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
          // paddingTop: "60px", // added this due to making header position: fixed
          display: "flex",
          flexDirection: "column",
          flexGrow: 1,
        }}
      >
        <HeaderNew tabs={tabs} />
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
              {/* <AnimatePresence mode="wait"> */}
              {showChildrenLeft ? (
                // <motion.div
                //   key={router.asPath}
                //   initial={{ x: 100, opacity: 0 }}
                //   animate={{ x: 0, opacity: 1 }}
                //   exit={{ x: 100, opacity: 0 }}
                //   transition={{ duration: 0.4, ease: "easeOut" }}
                //   style={{ height: "100%" }}
                // >
                children
              ) : (
                // </motion.div>

                <Container
                  maxWidth="xl"
                  sx={{
                    px: { xs: 1, sm: 2, md: 3 },
                    pb: "200px",
                    mt: "24px",
                  }}
                >
                  <Typography component="h2" variant="h5" sx={{ mb: "1.5rem" }}>
                    Listen Live
                  </Typography>
                  <HydrophonesStack />
                </Container>
              )}
              {/* </AnimatePresence> */}
            </SideList>
          )}
          {!mdDown && <MapWrapper masterPlayerTimeRef={masterPlayerTimeRef} />}
          {!mdDown && (
            <SideList>
              {isFeedDetail ? (
                <HydrophoneCandidatesPage />
              ) : showChildrenRight ? (
                children
              ) : (
                <Container
                  maxWidth="xl"
                  sx={{
                    px: { xs: 1, sm: 2, md: 3 },
                    pb: "200px",
                    mt: "24px",
                  }}
                >
                  <Typography component="h2" variant="h5" sx={{ mb: "1rem" }}>
                    Reports
                  </Typography>
                  <CandidatesStack />
                </Container>
              )}
            </SideList>
          )}
          {/* // mobile view */}
          {mdDown && (
            <>
              {showChildrenLeft ? (
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
