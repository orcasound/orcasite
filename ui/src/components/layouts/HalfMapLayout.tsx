import { Earbuds, Mic } from "@mui/icons-material";
import {
  BottomNavigation,
  BottomNavigationAction,
  Box,
  Container,
  Theme,
  useMediaQuery,
} from "@mui/material";
import { type Map as LeafletMap } from "leaflet";
import dynamic from "next/dynamic";
import { useRouter } from "next/router";
import {
  ReactElement,
  ReactNode,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";

import Header from "@/components/Header";
import { useData } from "@/context/DataContext";
import { LayoutContext } from "@/context/LayoutContext";
import { useNowPlaying } from "@/context/NowPlayingContext";

import CandidatesTabs from "../CandidateList/CandidatesTabs";
import {
  CandidatesStack,
  VisualizationsStack,
} from "../CandidateList/CandidatesTabs";
import PlayerTimeDisplay from "../CandidateList/PlayerTimeDisplay";
import PlayBar from "../PlayBar";
import { MasterDataLayout } from "./MasterDataLayout";

const MapWithNoSSR = dynamic(() => import("@/components/NewMap"), {
  ssr: false,
});

// TODO: need a URL that shows candidate map closeup, without the detail overlay
// const feedFromSlug = (feedSlug: string) => ({
//   id: feedSlug,
//   name: feedSlug,
//   slug: feedSlug,
//   nodeName: feedSlug,
//   // TODO: pass in bucket from dynamic feed instead of env/hardcoding
//   bucket: process.env.NEXT_PUBLIC_S3_BUCKET ?? "audio-orcasound-net",
//   // TODO: figure out which coordinates to use for dynamic feeds
//   latLng: { lat: 48.6, lng: -122.3 },
// });

function HalfMapLayout({ children }: { children: ReactNode }) {
  const router = useRouter();
  // const slug = router.query.feed as string;
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const { nowPlaying } = useNowPlaying();
  const { feeds } = useData();
  console.log("rendering halfmap");

  const nowPlayingFeed = useMemo(() => {
    if (!nowPlaying?.array?.[0]) return undefined;
    return feeds.find((feed) => feed.id === nowPlaying.array[0].feedId);
  }, [nowPlaying, feeds]);

  const masterPlayerTimeRef = useRef(0);
  // const [masterPlayerTime, setMasterPlayerTime] = useState(0);

  const [menuTab, setMenuTab] = useState(0);
  const menu = (
    <BottomNavigation
      showLabels
      value={menuTab}
      onChange={(event, newMenuTab) => {
        setMenuTab(newMenuTab);
      }}
      sx={{ height: "69px" }}
    >
      <BottomNavigationAction label="Recordings" icon={<Earbuds />} />
      <BottomNavigationAction label="Listen Live" icon={<Mic />} />
    </BottomNavigation>
  );

  type MobileContainerProps = {
    children: React.ReactNode;
  };

  const MobileContainer = ({ children }: MobileContainerProps) => {
    return (
      <Container
        maxWidth="xl"
        sx={{
          px: { xs: 1, sm: 2, md: 3 },
        }}
      >
        {children}
      </Container>
    );
  };

  const mapTitle = (
    <Box
      sx={{
        position: "absolute",
        top: 20,
        left: 20,
        width: "300px",
        zIndex: 10000,
        display: "flex",
        flexDirection: "column",
        gap: "4px",
      }}
    >
      <PlayerTimeDisplay
        nowPlaying={nowPlaying}
        masterPlayerTimeRef={masterPlayerTimeRef}
      />
    </Box>
  );

  // const mapTitle = (
  //   <Typography
  //     id="map-title"
  //     sx={{
  //       position: "absolute",
  //       color: "black",
  //       right: 0,
  //       zIndex: 10000,
  //     }}
  //   >
  //     {formatTimestamp(nowPlaying?.array?.[0].timestamp)}
  //   </Typography>
  // );

  // const isDynamic = router.asPath.split("/")[1] === "dynamic";
  // // don't make feed request if there's no feed slug or is dynamic
  // const feedFromQuery = useFeedQuery(
  //   { slug: slug },
  //   { enabled: !!slug || isDynamic },
  // ).data?.feed;
  // const feed = isDynamic ? feedFromSlug(slug) : feedFromQuery;

  const [map, setMap] = useState<LeafletMap>();
  // const feeds = useFeedsQuery().data?.feeds ?? [];
  const firstOnlineFeed = feeds.filter(({ online }) => online)[0];
  const currentFeed = useMemo(() => {
    return nowPlayingFeed ? nowPlayingFeed : firstOnlineFeed;
  }, [nowPlayingFeed, firstOnlineFeed]);
  const [tabValue, setTabValue] = useState(0);

  const handleChange = (event: React.MouseEvent<HTMLDivElement>) => {
    setTabValue(Number(event.currentTarget.id));
  };

  // update the currentFeed only if there's a new feed
  useEffect(() => {
    // console.log("nowPlaying: " + JSON.stringify(nowPlaying, null, 2))
    // console.log("currentFeed: " + JSON.stringify(currentFeed, null, 2))
    // console.log("nowPlayingFeed: " + JSON.stringify(nowPlayingFeed, null, 2))
    if (
      nowPlayingFeed
      // && feed.slug !== currentFeed?.slug
    ) {
      // setCurrentFeed(currentFeed);
      map?.setZoom(12);
      // map?.panTo(nowPlayingFeed.latLng);
    } else {
      map?.setZoom(8);
    }
    // if (!feed && !currentFeed && firstOnlineFeed) {
    //   setCurrentFeed(firstOnlineFeed);
    // }
  }, [map, currentFeed, firstOnlineFeed, nowPlaying, nowPlayingFeed]);

  useEffect(() => {
    if (nowPlayingFeed) {
      map?.panTo(nowPlayingFeed.latLng);
    }
  }, [nowPlayingFeed, map]);

  const mapBox = (
    <Box sx={{ flexGrow: 1 }}>
      {mapTitle}
      <MapWithNoSSR
        setMap={setMap}
        currentFeed={nowPlayingFeed}
        feeds={feeds}
      />
    </Box>
  );

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
        paddingBottom: smDown ? "125px" : "80px",
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
        {!mdDown && (
          <Box
            className="side-list"
            sx={{
              borderRightColor: "divider",
              borderRightStyle: "solid",
              borderRightWidth: 1,
              width: "45%",
              overflow: "auto",
            }}
          >
            <CandidatesTabs />
          </Box>
        )}
        <Box
          sx={{
            minHeight: "44px",
            display: "flex",
            justifyContent: "center",
            alignItems: "center",
            gap: "2rem",
          }}
        >
          {mdDown && menuTab === 0 && (
            <>
              <Box id="0" onClick={handleChange}>
                Map
              </Box>
              <Box id="1" onClick={handleChange}>
                Candidates
              </Box>
              <Box id="2" onClick={handleChange}>
                Visualizations
              </Box>
            </>
          )}
          {mdDown && menuTab === 1 && (
            <>
              <Box id="0" onClick={handleChange}>
                Hydrophones
              </Box>
              <Box id="1" onClick={handleChange}>
                Map
              </Box>
            </>
          )}
        </Box>
        <Box
          className="main-panel"
          sx={{
            flex: 1,
            display: "flex",
            flexDirection: "column",
            minWidth: 0,
            position: "relative",
          }}
        >
          {
            // desktop index page, show map
            !mdDown && router.query.candidateId === undefined ? (
              mapBox
            ) : mdDown &&
              router.query.candidateId === undefined &&
              menuTab === 0 ? (
              tabValue === 0 ? (
                mapBox
              ) : tabValue === 1 ? (
                <MobileContainer>
                  <CandidatesStack />
                </MobileContainer>
              ) : (
                <MobileContainer>
                  <VisualizationsStack />
                </MobileContainer>
              )
            ) : // mobile index page, listen live
            mdDown &&
              router.query.candidateId === undefined &&
              menuTab === 1 ? (
              tabValue === 1 ? (
                mapBox
              ) : (
                <div
                  style={{
                    width: "100%",
                    height: "100%",
                    display: "flex",
                    alignItems: "center",
                    justifyContent: "center",
                  }}
                >
                  hydrophone list
                </div>
              )
            ) : (
              // detail page content, mobile or desktop
              children
            )
          }
        </Box>
      </Box>
      <PlayBar mobileMenu={menu} masterPlayerTimeRef={masterPlayerTimeRef} />
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
