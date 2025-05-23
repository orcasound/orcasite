import {
  Box,
  Container,
  Stack,
  Tab,
  Tabs,
  Theme,
  useMediaQuery,
} from "@mui/material";
import { useRouter } from "next/router";
import React from "react";

import { CandidatesStack } from "./CandidatesStack";
import { HydrophonesStack } from "./HydrophonesStack";
import { VisualizationsStack } from "./VisualizationsStack";

const tabSlugs = ["hydrophones", "candidates", "visualizations"];

function getTabIndexFromPath(path: string): number {
  const slug = path.replace("/beta/", "");
  if (tabSlugs.includes(slug)) {
    return tabSlugs.indexOf(slug);
  } else {
    return 0;
  }
}

export default function DesktopTabs() {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const router = useRouter();
  const tabIndex = getTabIndexFromPath(router.route);

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

  const handleChange = (event: React.SyntheticEvent, newValue: number) => {
    router.push(`/beta/${tabSlugs[newValue]}`, undefined, { shallow: true });
  };

  const tabs = (
    <Stack>
      <Tabs
        value={tabIndex}
        onChange={handleChange}
        aria-label="navigation tabs"
        centered={mdDown ? true : false}
      >
        <Tab label="Hydrophones" {...a11yProps(0)} />
        <Tab label="Candidates" {...a11yProps(1)} />
        <Tab label="Visualizations" {...a11yProps(2)} />
      </Tabs>
      <DesktopTabPanel value={tabIndex} index={0}>
        <HydrophonesStack />
      </DesktopTabPanel>
      <DesktopTabPanel value={tabIndex} index={1}>
        <CandidatesStack />
      </DesktopTabPanel>
      <DesktopTabPanel value={tabIndex} index={2}>
        <VisualizationsStack />
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
        {tabs}
      </Box>
    </Container>
  );
}
