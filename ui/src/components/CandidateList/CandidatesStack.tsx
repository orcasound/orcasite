import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import React from "react";

import CandidateListFilters from "./CandidateListFilters";
import CandidatesList from "./CandidatesList";
import { CandidatesResults } from "./CandidatesResults";

export const CandidatesStack = () => {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  return (
    <Stack>
      <CandidateListFilters />
      <Box sx={{ paddingTop: "1.5rem", overflow: mdDown ? "auto" : "initial" }}>
        <CandidatesResults viewType="list" />
        <Box sx={{ paddingTop: "1.5rem" }}></Box>
        <CandidatesList />
      </Box>
    </Stack>
  );
};
