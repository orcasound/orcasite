import {
  Box,
  Container,
  Stack,
  Tab,
  Tabs,
  Theme,
  useMediaQuery,
} from "@mui/material";
import React, { useState } from "react";

import { CandidatesStack } from "./CandidatesStack";
import { HydrophonesStack } from "./HydrophonesStack";
import { VisualizationsStack } from "./VisualizationsStack";

export default function CandidatesTabs({
  navOption,
}: {
  navOption?: "Listen Live" | "Recordings";
}) {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));

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

  // couldn't make this work, need to revisit
  function _CustomTab(props: TabProps) {
    const { label, index } = props;
    return <Tab label={label} {...a11yProps(index)} />;
  }

  function DesktopTabPanel(props: TabPanelProps) {
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
      <DesktopTabPanel value={tabValue} index={0}>
        <CandidatesStack />
      </DesktopTabPanel>
      <DesktopTabPanel value={tabValue} index={1}>
        <VisualizationsStack />
      </DesktopTabPanel>
      <DesktopTabPanel value={tabValue} index={2}>
        <HydrophonesStack />
      </DesktopTabPanel>
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
        {desktopTabs}
        {/* {mdDown && navOption === "Listen Live"
          ? listenLiveTabs
          : mdDown && navOption === "Recordings"
            ? recordingsTabs
            : desktopTabs} */}
      </Box>
    </Container>
  );
}
