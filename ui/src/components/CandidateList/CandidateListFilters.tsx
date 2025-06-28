import { Tune } from "@mui/icons-material";
import { Button, Stack, Theme, useMediaQuery } from "@mui/material";
import { SelectChangeEvent } from "@mui/material/Select";
import dayjs, { Dayjs } from "dayjs";
import { Dispatch, SetStateAction, useState } from "react";

import { useData } from "@/context/DataContext";
import {
  allTime,
  customRange,
  oneDay,
  sevenDays,
  threeDays,
} from "@/utils/masterDataHelpers";

import ChartSelect from "./ChartSelect";
import { CustomDatePicker } from "./CustomDatePicker";
import SearchBar from "./SearchBar";

export const defaultRange = sevenDays;

export const timeRangeSelect = [
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
    label: "Group reports within 5 min",
    value: 5,
  },
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
    label: "Whale + Whale (AI) + Sightings",
    value: "whale + whale (ai) + sightings",
  },
  {
    label: "Sightings",
    value: "sighting",
  },
];

const numberDetectionsSelect = [
  {
    label: "1+ detections",
    value: 1,
  },
  {
    label: "2+ detections",
    value: 2,
  },
  {
    label: "3+ detections",
    value: 3,
  },
  {
    label: "4+ detections",
    value: 4,
  },
  {
    label: "5+ detections",
    value: 5,
  },
];

const CandidateListFilters = ({
  showTimeRange = true,
  showFilterButton = true,
  showFilters = true,
  setShowFilters,
}: {
  showTimeRange?: boolean;
  showFilterButton?: boolean;
  showFilters?: boolean;
  setShowFilters?: Dispatch<SetStateAction<boolean>>;
}) => {
  const { feeds, filters, setFilters } = useData();
  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));

  // dynamically create hydrophone options
  const feedList = feeds.map((el) => ({
    label: el.name,
    value: el.name,
  }));
  feedList.unshift({ label: "All hydrophones", value: "All hydrophones" });

  if (mdDown) {
    showFilterButton = false;
  }
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

  const handleToggle = () => {
    if (setShowFilters) setShowFilters(!showFilters);
  };

  return (
    <Stack
      style={{
        display: "flex",
        gap: "1rem",
        flexWrap: "wrap",
        width: "100%",
        alignItems: "flex-end",
      }}
    >
      <Stack direction="row" spacing={2} sx={{ width: "100%" }}>
        {showTimeRange && (
          <ChartSelect
            name={"timeRange"}
            value={filters.timeRange}
            list={timeRangeSelect}
            onChange={handleChange}
          />
        )}
        {showFilterButton && (
          <Button
            sx={{ minWidth: "auto" }}
            startIcon={
              <Tune
                sx={{
                  color: showFilters ? "background.paper" : "text.primary",
                }}
              />
            }
            size="small"
            variant={showFilters ? "contained" : "outlined"}
            onClick={handleToggle}
          >
            Filters
          </Button>
        )}
        {!mdDown && (
          <>
            <Button
              href="#"
              size="small"
              variant="outlined"
              sx={{ whiteSpace: "nowrap", maxHeight: "31px" }}
            >
              Sign up
            </Button>
            <Button
              href="#"
              size="small"
              variant="contained"
              sx={{ whiteSpace: "nowrap", maxHeight: "31px" }}
            >
              Log in
            </Button>
          </>
        )}
      </Stack>

      {showFilters && (
        <Stack
          direction={mdDown ? "column" : "row"}
          spacing={2}
          sx={{ maxWidth: "100%" }}
        >
          <SearchBar />
          {/* <ChartSelect
            name={"hydrophone"}
            value={filters.hydrophone}
            list={feedList}
            onChange={handleChange}
          /> */}
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
          <ChartSelect
            name={"detectionsGreaterThan"}
            value={filters.detectionsGreaterThan}
            list={numberDetectionsSelect}
            onChange={handleChange}
          />
        </Stack>
      )}

      {filters.timeRange === customRange && (
        <Stack direction="row" spacing={2} sx={{ maxWidth: "100%" }}>
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
        </Stack>
      )}
    </Stack>
  );
};

export default CandidateListFilters;
