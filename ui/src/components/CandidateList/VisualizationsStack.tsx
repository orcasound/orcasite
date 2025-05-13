import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import React from "react";

import ReportsBarChart from "@/components/CandidateList/ReportsBarChart";

import CandidateListFilters from "./CandidateListFilters";
import { CandidatesResults } from "./CandidatesResults";

export const VisualizationsStack = () => {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  return (
    <Stack>
      <CandidateListFilters />
      <Box sx={{ paddingTop: "1.5rem", overflow: mdDown ? "auto" : "initial" }}>
        <CandidatesResults viewType="chart" />
        <Box sx={{ paddingTop: "1.5rem" }}></Box>
        <ReportsBarChart />
      </Box>
    </Stack>
  );
};
