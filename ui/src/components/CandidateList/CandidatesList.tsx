import type { Theme } from "@mui/material";
import { Box, Container, Stack, Typography } from "@mui/material";
import { useMediaQuery } from "@mui/material";
import dayjs from "dayjs";
import { Dayjs } from "dayjs";
import isSameOrAfter from "dayjs/plugin/isSameOrAfter";
import isSameOrBefore from "dayjs/plugin/isSameOrBefore";
import { useEffect, useState } from "react";

import CandidateCard from "@/components/CandidateList/CandidateCard";
import ChartSelect from "@/components/CandidateList/ChartSelect";
import ReportsBarChart from "@/components/CandidateList/ReportsBarChart";
import { useData } from "@/context/DataContext";
import { useLayout } from "@/context/LayoutContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { Candidate, CombinedData } from "@/types/DataTypes";

import CandidateListFilters from "./CandidateListFilters";
import { defaultRange } from "./CandidateListFilters";
import useFilteredData from "./useFilteredData";
import useSortedCandidates from "./useSortedCandidates";

// dayjs plugin for date pickers
dayjs.extend(isSameOrBefore);
dayjs.extend(isSameOrAfter);

export type CandidateFilters = {
  timeRange: number;
  timeIncrement: number;
  hydrophone: string;
  category: string;
  startDate: Dayjs | null;
  endDate: Dayjs | null;
};

export default function CandidatesList() {
  const {
    combined,
    isSuccess,
  }: {
    combined: CombinedData[] | undefined;
    isSuccess: boolean;
  } = useData();

  const layout = useLayout();

  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  const [filters, setFilters] = useState<CandidateFilters>({
    timeRange: defaultRange,
    timeIncrement: 15,
    hydrophone: "All hydrophones",
    category: "All categories",
    startDate: null,
    endDate: dayjs(),
  });

  const [searchQuery, setSearchQuery] = useState("");

  const filteredData = useFilteredData(combined, filters, searchQuery);

  const [sortOrder, setSortOrder] = useState<"asc" | "desc">("desc");

  const sortedCandidates = useSortedCandidates(
    filteredData,
    filters.timeIncrement,
    sortOrder,
  );

  const { queue, setQueue } = useNowPlaying();
  useEffect(() => {
    setQueue(sortedCandidates);
  }, [sortedCandidates, setQueue]);
  useEffect(() => {
    console.log("queue length is: " + queue.length);
  }, [queue]);

  const candidateCards = sortedCandidates.map((candidate: Candidate) => (
    <CandidateCard
      candidate={candidate}
      key={
        candidate.array[0].timestampString +
        "-" +
        candidate.array[candidate.array.length - 1].timestampString
      }
    />
  ));

  return (
    <Container
      maxWidth="xl"
      sx={{
        px: { xs: 1, sm: 2, md: 3 },
      }}
    >
      <CandidateListFilters
        filters={filters}
        setFilters={setFilters}
        setSearchQuery={setSearchQuery}
      />
      <Box sx={{ paddingTop: "2rem" }}></Box>
      <Box
        sx={{
          display: "grid",
          gridAutoColumns: "1fr",
          gap: lgUp && layout === "leftNav" ? "40px" : 0,
          gridTemplateColumns:
            lgUp && layout === "leftNav" ? "1.42fr 1fr" : "1fr",
          gridTemplateRows: "auto",
        }}
      >
        <Stack
          sx={{
            // this is necessary as a grid item to avoid overflowing container
            minWidth: 0,
          }}
        >
          {!(lgUp && layout === "leftNav") && (
            <Box>
              <ReportsBarChart
                dataset={filteredData}
                timeRange={filters.timeRange}
              />
            </Box>
          )}
          <Box
            sx={{
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
            }}
          >
            <Typography>
              Showing {sortedCandidates.length}{" "}
              {!isSuccess ? "results, checking Orcahello..." : "results"}
            </Typography>
            <Box>
              <ChartSelect
                name="sortOrder"
                value={sortOrder}
                variant="standard"
                list={[
                  { label: "Newest first", value: "desc" },
                  { label: "Oldest first", value: "asc" },
                ]}
                onChange={(e) => {
                  setSortOrder(e.target.value as "asc" | "desc");
                }}
                sx={{
                  "&::before": { borderBottom: "none" },
                  "&::after": { borderBottom: "none" },
                  "&:hover:not(.Mui-disabled)::before": {
                    borderBottom: "none",
                  },
                  "& .MuiSelect-select": {
                    display: "flex",
                    alignItems: "center",
                    paddingY: "0.25rem",
                  },
                }}
              />
            </Box>
          </Box>
          <Box sx={{ paddingTop: "1rem" }}></Box>
          <Stack spacing={2}>{candidateCards}</Stack>
        </Stack>
        {lgUp && layout === "leftNav" && (
          <Box>
            <ReportsBarChart
              dataset={filteredData}
              timeRange={filters.timeRange}
            />
          </Box>
        )}
      </Box>
    </Container>
  );
}
