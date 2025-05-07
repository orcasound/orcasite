import {
  Box,
  Container,
  Stack,
  Tab,
  Tabs,
  Theme,
  useMediaQuery,
} from "@mui/material";
import type { Map as LeafletMap } from "leaflet";
import dynamic from "next/dynamic";
import React, { useEffect, useMemo, useState } from "react";

import ReportsBarChart from "@/components/CandidateList/ReportsBarChart";
import { Feed } from "@/graphql/generated";
import { Candidate } from "@/types/DataTypes";

import CandidateListFilters from "./CandidateListFilters";
import CandidatesList from "./CandidatesList";
import { CandidatesResults } from "./CandidatesResults";

const MapWithNoSSR = dynamic(() => import("@/components/NewMap"), {
  ssr: false,
});

export default function CandidatesTabs({
  nowPlaying,
  feeds,
  navOption,
}: {
  nowPlaying?: Candidate;
  feeds: Feed[];
  navOption?: "Listen Live" | "Recordings";
}) {
  console.log("rendering CandidatesTabs");
  useEffect(() => {
    console.log("tab: " + navOption);
  }, [navOption]);
  // tabs
  interface TabProps {
    index: number;
    label: string;
  }

  interface TabPanelProps {
    children?: React.ReactNode;
    index: number;
    value: number;
  }

  const nowPlayingFeed = useMemo(() => {
    if (!nowPlaying?.array?.[0]) return undefined;
    return feeds?.find((feed) => feed.id === nowPlaying.array[0].feedId);
  }, [nowPlaying, feeds]);

  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const [map, setMap] = useState<LeafletMap>();
  // const firstOnlineFeed = feeds?.filter(({ online }) => online)[0];
  // const [currentFeed, setCurrentFeed] = useState(
  //   nowPlayingFeed ? nowPlayingFeed : firstOnlineFeed,
  // );

  const mapBox = useMemo(
    () => (
      <Box sx={{ flexGrow: 1 }}>
        <MapWithNoSSR
          setMap={setMap}
          currentFeed={nowPlayingFeed}
          feeds={feeds}
        />
      </Box>
    ),
    [feeds, nowPlayingFeed],
  );

  // couldn't make this work, need to revisit
  function _CustomTab(props: TabProps) {
    const { label, index } = props;
    return <Tab label={label} {...a11yProps(index)} />;
  }

  function CustomTabPanel(props: TabPanelProps) {
    const { children, value, index, ...other } = props;
    return (
      <div
        role="tabpanel"
        hidden={value !== index}
        id={`simple-tabpanel-${index}`}
        aria-labelledby={`simple-tab-${index}`}
        style={{ marginTop: "24px" }}
        {...other}
      >
        {value === index && <Box sx={{ p: 0 }}>{children}</Box>}
      </div>
    );
  }

  function a11yProps(index: number) {
    return {
      id: `simple-tab-${index}`,
      "aria-controls": `simple-tabpanel-${index}`,
    };
  }

  const initialTabValue = navOption === "Recordings" ? 1 : 0;
  const [tabValue, setTabValue] = useState(initialTabValue);

  const handleChange = (event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
  };

  const candidateList = (
    <Stack>
      <CandidateListFilters />
      <Box sx={{ paddingTop: "1.5rem" }}></Box>
      <CandidatesResults viewType="list" />
      <Box sx={{ paddingTop: "1.5rem" }}></Box>
      <CandidatesList />
    </Stack>
  );

  const visualizations = (
    <Stack>
      <CandidateListFilters />
      <Box sx={{ paddingTop: "1.5rem" }}></Box>
      <CandidatesResults viewType="chart" />
      <Box sx={{ paddingTop: "1.5rem" }}></Box>
      <ReportsBarChart />
    </Stack>
  );

  const desktopTabs = (
    <Stack>
      <Tabs
        value={tabValue}
        onChange={handleChange}
        aria-label="navigation tabs"
        centered={mdDown ? true : false}
      >
        <Tab label="Candidates" {...a11yProps(0)} />
        <Tab label="Visualizations" {...a11yProps(1)} />
        <Tab label="Hydrophones" {...a11yProps(2)} />
      </Tabs>
      <CustomTabPanel value={tabValue} index={0}>
        {candidateList}
      </CustomTabPanel>
      <CustomTabPanel value={tabValue} index={1}>
        {visualizations}
      </CustomTabPanel>
      <CustomTabPanel value={tabValue} index={2}>
        {/* add hydrophone list */}
      </CustomTabPanel>
    </Stack>
  );

  const listenLiveTabs = (
    <Stack>
      <Tabs
        value={tabValue}
        onChange={handleChange}
        aria-label="navigation tabs"
        centered={mdDown ? true : false}
      >
        <Tab label="Hydrophones" {...a11yProps(0)} />
        <Tab label="Map" {...a11yProps(1)} />
      </Tabs>
      <CustomTabPanel value={tabValue} index={0}></CustomTabPanel>
      <CustomTabPanel value={tabValue} index={1}>
        {/* add hydrophone list */}
      </CustomTabPanel>
    </Stack>
  );

  const recordingsTabs = (
    <Stack>
      <Tabs
        value={tabValue}
        onChange={handleChange}
        aria-label="navigation tabs"
        centered={mdDown ? true : false}
      >
        <Tab label="Candidates" {...a11yProps(0)} />
        <Tab label="Visualizations" {...a11yProps(1)} />
        <Tab label="Map" {...a11yProps(2)} />
      </Tabs>
      <CustomTabPanel value={tabValue} index={0}>
        {candidateList}
      </CustomTabPanel>
      <CustomTabPanel value={tabValue} index={1}>
        {visualizations}
      </CustomTabPanel>
      <CustomTabPanel value={tabValue} index={2}>
        {tabValue === 2 && mapBox}
      </CustomTabPanel>
    </Stack>
  );

  return (
    <Container
      maxWidth="xl"
      sx={{
        px: { xs: 1, sm: 2, md: 3 },
        pb: "200px",
      }}
    >
      <Box
        sx={{
          borderBottom: 0,
          borderColor: "accent1",
          marginTop: mdDown ? 0 : "1rem",
        }}
      >
        {mdDown && navOption === "Listen Live"
          ? listenLiveTabs
          : mdDown && navOption === "Recordings"
            ? recordingsTabs
            : desktopTabs}
      </Box>
    </Container>
  );
}
