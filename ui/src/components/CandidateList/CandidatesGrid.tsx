import type { Theme } from "@mui/material";
import { Box, Container, Stack } from "@mui/material";
import { useMediaQuery } from "@mui/material";

import ReportsBarChart from "@/components/CandidateList/ReportsBarChart";

import CandidateListFilters from "./CandidateListFilters";
import CandidatesList from "./CandidatesList";
import { CandidatesResults } from "./CandidatesResults";

export default function CandidatesGrid() {
  // layout
  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  return (
    <Container
      className="container-in-question"
      maxWidth="xl"
      sx={{
        px: { xs: 1, sm: 2, md: 3 },
      }}
    >
      <CandidateListFilters />
      <Box sx={{ paddingTop: "2rem" }}></Box>
      <Box
        className="grid-box"
        sx={{
          display: "grid",
          gridAutoColumns: "1fr",
          gap: lgUp ? "40px" : 0,
          gridTemplateColumns: lgUp ? "1.42fr 1fr" : "1fr",
          gridTemplateRows: "auto",
        }}
      >
        <Stack
          sx={{ minWidth: 0 }}
          // minWidth is necessary as a grid item to avoid overflowing container
        >
          <CandidatesResults viewType="list" />
          <Box sx={{ paddingTop: "1rem" }}></Box>
          <CandidatesList />
        </Stack>
        <Stack sx={{ minWidth: 0 }}>
          <CandidatesResults viewType="chart" layout="grid" />
          <Box sx={{ paddingTop: "1rem" }}></Box>
          <ReportsBarChart />
        </Stack>
      </Box>
    </Container>
  );
}
