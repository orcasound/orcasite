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
import React, { useState } from "react";

import ReportsBarChart from "@/components/CandidateList/ReportsBarChart";
import { useData } from "@/context/DataContext";

import CandidateListFilters from "./CandidateListFilters";
import CandidatesList from "./CandidatesList";
import { CandidatesResults } from "./CandidatesResults";

const MapWithNoSSR = dynamic(() => import("@/components/Map"), {
  ssr: false,
});

export default function CandidatesTabs({
  mapBox,
  tab,
}: {
  mapBox?: React.ReactNode;
  tab?: "Listen Live" | "Recordings";
}) {
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

  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const [map, setMap] = useState<LeafletMap>();
  const { feeds } = useData();

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

  const [tabValue, setTabValue] = useState(0);
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
      <CustomTabPanel value={tabValue} index={2}></CustomTabPanel>
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
        {mdDown && tab === "Listen Live"
          ? listenLiveTabs
          : mdDown && tab === "Recordings"
            ? recordingsTabs
            : desktopTabs}
      </Box>
    </Container>
  );
}
