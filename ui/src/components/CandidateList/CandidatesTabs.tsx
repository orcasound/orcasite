import { Box, Container, Tab, Tabs } from "@mui/material";
import { useState } from "react";

import ReportsBarChart from "@/components/CandidateList/ReportsBarChart";

import CandidateListFilters from "./CandidateListFilters";
import CandidatesList from "./CandidatesList";
import { CandidatesResults } from "./CandidatesResults";

export default function CandidatesTabs() {
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

  function CustomTabPanel(props: TabPanelProps) {
    const { children, value, index, ...other } = props;
    return (
      <div
        role="tabpanel"
        hidden={value !== index}
        id={`simple-tabpanel-${index}`}
        aria-labelledby={`simple-tab-${index}`}
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

  return (
    <Container
      maxWidth="xl"
      sx={{
        px: { xs: 1, sm: 2, md: 3 },
      }}
    >
      <Box
        sx={{ borderBottom: 0, borderColor: "accent1", margin: "1rem 0 24px" }}
      >
        <Tabs
          value={tabValue}
          onChange={handleChange}
          aria-label="navigation tabs"
        >
          <Tab label="Candidates" {...a11yProps(0)} />
          <Tab label="Visualizations" {...a11yProps(1)} />
          <Tab label="Hydrophones" {...a11yProps(2)} />
        </Tabs>
      </Box>
      <CustomTabPanel value={tabValue} index={0}>
        <CandidateListFilters />
        <Box sx={{ paddingTop: "1.5rem" }}></Box>
        <CandidatesResults viewType="list" />
        <Box sx={{ paddingTop: "1.5rem" }}></Box>
        <CandidatesList />
      </CustomTabPanel>
      <CustomTabPanel value={tabValue} index={1}>
        <CandidateListFilters />
        <Box sx={{ paddingTop: "1.5rem" }}></Box>
        <CandidatesResults viewType="chart" />
        <Box sx={{ paddingTop: "1.5rem" }}></Box>
        <ReportsBarChart />
      </CustomTabPanel>
    </Container>
  );
}
