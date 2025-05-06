import { Box, Typography } from "@mui/material";

import { useData } from "@/context/DataContext";

import ChartSelect from "./ChartSelect";

export function CandidatesResults({
  viewType,
  layout,
}: {
  viewType: "list" | "chart";
  layout?: string;
}) {
  const { setFilters, sortedCandidates, filters, isSuccess } = useData();

  const chartSelect = (
    <ChartSelect
      name="chartView"
      value={filters.chartLegend}
      variant="standard"
      fontSize="14px"
      list={[
        { label: "By category", value: "category" },
        { label: "By hydrophone", value: "hydrophone" },
      ]}
      onChange={(e) => {
        setFilters((prev) => ({
          ...prev,
          chartLegend: e.target.value as "category" | "hydrophone",
        }));
      }}
      sx={{
        "& .MuiSelect-select": {
          display: "flex",
          alignItems: "center",
          paddingY: "0.25rem",
        },
      }}
    />
  );

  const listSelect = (
    <ChartSelect
      name="sortOrder"
      value={filters.sortOrder}
      variant="standard"
      fontSize="14px"
      list={[
        { label: "Newest first", value: "desc" },
        { label: "Oldest first", value: "asc" },
      ]}
      onChange={(e) => {
        setFilters((prev) => ({
          ...prev,
          sortOrder: e.target.value as "asc" | "desc",
        }));
      }}
      sx={{
        "& .MuiSelect-select": {
          display: "flex",
          alignItems: "center",
          paddingY: "0.25rem",
        },
      }}
    />
  );

  return (
    <Box
      sx={{
        display: "flex",
        justifyContent: layout !== "grid" ? "space-between" : "flex-end",
        alignItems: "center",
      }}
    >
      {layout !== "grid" && (
        <Typography sx={{ fontSize: "14px" }}>
          Showing {sortedCandidates.length}{" "}
          {!isSuccess ? "results, checking Orcahello..." : "results"}
        </Typography>
      )}

      <Box>{viewType === "list" ? listSelect : chartSelect}</Box>
    </Box>
  );
}
