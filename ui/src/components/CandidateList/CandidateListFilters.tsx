import { Box } from "@mui/material";
import { SelectChangeEvent } from "@mui/material/Select";
import dayjs, { Dayjs } from "dayjs";
import { useState } from "react";

import { useData } from "@/context/DataContext";

import { CandidateFilters } from "./CandidatesList";
import ChartSelect from "./ChartSelect";
import { CustomDatePicker } from "./CustomDatePicker";
import SearchBar from "./SearchBar";

const sevenDays = 7 * 24 * 60 * 60 * 1000;
const threeDays = 3 * 24 * 60 * 60 * 1000;
const oneDay = 24 * 60 * 60 * 1000;
export const allTime = -1;
export const customRange = -2;
export const defaultRange = sevenDays;

const timeRangeSelect = [
  {
    label: "All time",
    value: allTime,
  },
  {
    label: "Last 7 days",
    value: sevenDays,
  },
  {
    label: "Last 3 days",
    value: threeDays,
  },
  {
    label: "Last 24 hours",
    value: oneDay,
  },
  {
    label: "Custom",
    value: customRange,
  },
];

const timeIncrementSelect = [
  {
    label: "Group reports within 15 min",
    value: 15,
  },
  {
    label: "Group reports within 30 min",
    value: 30,
  },
  {
    label: "Group reports within 60 min",
    value: 60,
  },
  {
    label: "Do not group reports",
    value: 0,
  },
];

const categorySelect = [
  {
    label: "All categories",
    value: "All categories",
  },
  {
    label: "Whale",
    value: "whale",
  },
  {
    label: "Vessel",
    value: "vessel",
  },
  {
    label: "Other",
    value: "other",
  },
  {
    label: "Whale (AI)",
    value: "whale (ai)",
  },
  {
    label: "Whale + Whale (AI)",
    value: "whale + whale (ai)",
  },
];

const CandidateListFilters = ({
  filters,
  setFilters,
  setSearchQuery,
}: {
  filters: CandidateFilters;
  setFilters: React.Dispatch<React.SetStateAction<CandidateFilters>>;
  setSearchQuery: React.Dispatch<React.SetStateAction<string>>;
}) => {
  const { feeds } = useData();
  const feedList = feeds.map((el) => ({
    label: el.name,
    value: el.name,
  }));

  feedList.unshift({ label: "All hydrophones", value: "All hydrophones" });

  const [lastPredefinedRange, setLastPredefinedRange] =
    useState<number>(defaultRange);

  const handleChange = (event: SelectChangeEvent<unknown>) => {
    const { name, value } = event.target;

    setFilters((prevFilters) => {
      const newFilters = {
        ...prevFilters,
        [name]: value,
      };

      if (name === "timeRange") {
        if (value !== customRange) {
          // Save the last predefined range
          setLastPredefinedRange(value as number);
          newFilters.startDate = null;
          newFilters.endDate = dayjs(); // now
        } else {
          // Prefill based on last predefined range
          newFilters.startDate =
            lastPredefinedRange < 0
              ? null
              : dayjs().subtract(lastPredefinedRange, "millisecond");
          newFilters.endDate = dayjs();
        }
      }

      return newFilters;
    });
  };

  const handleDatePicker = (name: string, value: Dayjs | null) => {
    setFilters((prevFilters) => ({
      ...prevFilters,
      [name]: value,
    }));
  };

  return (
    <Box
      style={{
        display: "flex",
        margin: "24px 0",
        gap: "1rem",
        flexWrap: "wrap",
        width: "100%",
      }}
    >
      <SearchBar setSearchQuery={setSearchQuery} />
      <ChartSelect
        name={"timeRange"}
        value={filters.timeRange}
        list={timeRangeSelect}
        onChange={handleChange}
      />

      {filters.timeRange === customRange && (
        <>
          <CustomDatePicker
            label="Start date"
            valueProp={filters.startDate}
            name="startDate"
            onDataChange={handleDatePicker}
          />
          <CustomDatePicker
            label="End date"
            valueProp={filters.endDate}
            name="endDate"
            onDataChange={handleDatePicker}
          />
        </>
      )}

      <ChartSelect
        name={"hydrophone"}
        value={filters.hydrophone}
        list={feedList}
        onChange={handleChange}
      />
      <ChartSelect
        name={"category"}
        value={filters.category}
        list={categorySelect}
        onChange={handleChange}
      />
      <ChartSelect
        name={"timeIncrement"}
        value={filters.timeIncrement}
        list={timeIncrementSelect}
        onChange={handleChange}
      />
    </Box>
  );
};

export default CandidateListFilters;
