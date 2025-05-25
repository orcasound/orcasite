import { Box, Stack, Theme, useMediaQuery } from "@mui/material";
import React from "react";

import { useData } from "@/context/DataContext";

import CandidateListFilters from "./CandidateListFilters";
import HydrophoneCard from "./HydrophoneCard";

export const HydrophonesStack = () => {
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const { feeds } = useData();

  return (
    <Stack>
      <CandidateListFilters />
      <Box sx={{ paddingTop: "1.5rem", overflow: mdDown ? "auto" : "initial" }}>
        <Stack spacing={2}>
          {feeds.map((feed) => {
            return <HydrophoneCard key={feed.id} feed={feed} />;
          })}
        </Stack>
      </Box>
    </Stack>
  );
};
