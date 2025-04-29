import type { Theme } from "@mui/material";
import { Box, Container, Stack, Typography } from "@mui/material";
import { useMediaQuery } from "@mui/material";
import { SelectChangeEvent } from "@mui/material/Select";
import dayjs from "dayjs";
import { Dayjs } from "dayjs";
import isSameOrAfter from "dayjs/plugin/isSameOrAfter";
import isSameOrBefore from "dayjs/plugin/isSameOrBefore";
import { useEffect, useMemo, useRef, useState } from "react";

import CandidateCard from "@/components/CandidateCard";
import ChartSelect from "@/components/ChartSelect";
import { CustomDatePicker } from "@/components/CustomDatePicker";
import ReportsBarChart from "@/components/ReportsBarChart";
import SearchBar from "@/components/SearchBar";
import { useData } from "@/context/DataContext";
import { useLayout } from "@/context/LayoutContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { Feed } from "@/graphql/generated";
import { Candidate, CombinedData } from "@/types/DataTypes";

// dayjs plugin for date pickers
dayjs.extend(isSameOrBefore);
dayjs.extend(isSameOrAfter);

const sevenDays = 7 * 24 * 60 * 60 * 1000;
const threeDays = 3 * 24 * 60 * 60 * 1000;
const oneDay = 24 * 60 * 60 * 1000;
const allTime = -1;
const customRange = -2;
const defaultRange = sevenDays;

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

const createCandidates = (
  dataset: CombinedData[],
  interval: number,
): Candidate[] => {
  const candidates: Array<Array<CombinedData>> = [];
  const sort = dataset.sort(
    (a, b) => Date.parse(b.timestampString) - Date.parse(a.timestampString),
  );
  sort.forEach((el: CombinedData) => {
    if (!candidates.length) {
      const firstArray = [];
      firstArray.push(el);
      candidates.push(firstArray);
    } else {
      const hydrophone = el.hydrophone;
      const findLastMatchingArray = () => {
        for (let i = candidates.length - 1; i >= 0; i--) {
          if (candidates[i][0].hydrophone === hydrophone) {
            return candidates[i];
          }
        }
      };
      const lastMatchingArray = findLastMatchingArray();
      const lastTimestamp =
        lastMatchingArray &&
        lastMatchingArray[lastMatchingArray.length - 1].timestampString;
      if (
        lastTimestamp &&
        Math.abs(Date.parse(lastTimestamp) - Date.parse(el.timestampString)) /
          (1000 * 60) <=
          interval
      ) {
        lastMatchingArray.push(el);
      } else {
        const newArray = [];
        newArray.push(el);
        candidates.push(newArray);
      }
    }
  });
  const countCategories = (arr: { newCategory: string }[], cat: string) => {
    return arr.filter((d) => d.newCategory.toLowerCase() === cat).length;
  };

  const candidatesMap = candidates.map((candidate) => ({
    array: candidate,
    whale: countCategories(candidate, "whale"),
    vessel: countCategories(candidate, "vessel"),
    other: countCategories(candidate, "other"),
    "whale (AI)": countCategories(candidate, "whale (ai)"),
    hydrophone: candidate[0].hydrophone,
    descriptions: candidate
      .map((el: CombinedData) => el.comments)
      .filter((el: string | null | undefined) => el !== null)
      .join(" • "),
  }));

  return candidatesMap;
};

export default function CandidatesList() {
  const {
    combined,
    feeds,
    isSuccess,
  }: {
    combined: CombinedData[] | undefined;
    feeds: Feed[];
    isSuccess: boolean;
  } = useData();

  const { setQueue } = useNowPlaying();

  const layout = useLayout();

  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  const [filters, setFilters] = useState<{
    timeRange: number;
    timeIncrement: number;
    hydrophone: string;
    category: string;
    startDate: Dayjs | null;
    endDate: Dayjs | null;
  }>({
    timeRange: defaultRange,
    timeIncrement: 15,
    hydrophone: "All hydrophones",
    category: "All categories",
    startDate: null,
    endDate: dayjs(),
  });

  const [searchQuery, setSearchQuery] = useState("");
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

  // const [playing, setPlaying] = useState({
  //   index: -1,
  //   status: "ready",
  // });

  // const changeListState = (index: number, status: string) => {
  //   setPlaying((prevState) => ({
  //     ...prevState,
  //     index: index,
  //     status: status,
  //   }));
  // };

  // const [playNext, _setPlayNext] = useState(true);

  const players = useRef({});

  const feedList = feeds.map((el) => ({
    label: el.name,
    value: el.name,
  }));
  feedList.unshift({ label: "All hydrophones", value: "All hydrophones" });

  const filteredData = useMemo(() => {
    const min = Date.now() - filters.timeRange;

    return combined.filter((el: CombinedData) => {
      return (
        // uncomment this to block Orcahello data
        // el.type === "human" &&

        (filters.hydrophone === "All hydrophones" ||
          el.hydrophone === filters.hydrophone) &&
        (filters.category === "All categories" ||
          el.newCategory.toLowerCase() === filters.category ||
          (filters.category === "whale + whale (ai)" &&
            ["whale", "whale (ai)"].includes(el.newCategory.toLowerCase()))) &&
        (filters.timeRange === allTime ||
          filters.timeRange === customRange ||
          Date.parse(el.timestampString) >= min) &&
        (!filters.startDate ||
          dayjs(el.timestamp).isSameOrAfter(filters.startDate, "day")) &&
        dayjs(el.timestamp).isSameOrBefore(filters.endDate, "day") &&
        (searchQuery === "" ||
          (el.comments && el.comments.toLowerCase().includes(searchQuery)) ||
          el.newCategory.toLowerCase().includes(searchQuery) ||
          el.hydrophone.toLowerCase().includes(searchQuery))
      );
    });
  }, [combined, filters, searchQuery]);

  const [sortOrder, setSortOrder] = useState<"asc" | "desc">("desc");

  const candidates = useMemo(() => {
    return createCandidates(filteredData, filters.timeIncrement);
  }, [filteredData, filters.timeIncrement]);

  const [sortedCandidates, setSortedCandidates] = useState([...candidates]);

  // // clear players list whenever the candidates list changes
  useEffect(() => {
    players.current = {};
  }, [sortedCandidates, isSuccess]);

  useEffect(() => {
    const handledGetTime = (date?: Date) => {
      return date != null ? new Date(date).getTime() : 0;
    };

    const sortDescending = (array: Candidate[]) => {
      const sort = array.sort(
        (a, b) =>
          handledGetTime(b.array[0].timestamp) -
          handledGetTime(a.array[0].timestamp),
      );
      return sort;
    };

    const sortAscending = (array: Candidate[]) => {
      const sort = array.sort(
        (a, b) =>
          handledGetTime(a.array[0].timestamp) -
          handledGetTime(b.array[0].timestamp),
      );
      return sort;
    };
    const sorted =
      sortOrder === "desc"
        ? sortDescending([...candidates])
        : sortAscending([...candidates]);
    setSortedCandidates(sorted);
    setQueue(sorted);
  }, [candidates, sortOrder, isSuccess, setQueue]);

  const candidateCards = sortedCandidates.map(
    (candidate: Candidate, index: number) => (
      <CandidateCard
        candidate={candidate}
        key={
          candidate.array[0].timestampString +
          "-" +
          candidate.array[candidate.array.length - 1].timestampString
        }
        index={index}
        // changeListState={changeListState}
        // command={playing.index === index ? "play" : "pause"}
        feeds={feeds}
        players={players}
        // playNext={playNext}
      />
    ),
  );

  return (
    <Container
      maxWidth="xl"
      sx={{
        px: { xs: 1, sm: 2, md: 3 },
      }}
    >
      <Box
        style={{
          display: "flex",
          margin: "24px 0",
          gap: "1rem",
          flexWrap: "wrap",
          width: "100%",
        }}
      >
        {layout === "halfMap" ? "halfMap" : "leftNav"}
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
                feeds={feeds}
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
          <Stack spacing={3}>
            {candidateCards}
            {/* {sortedCandidates.length ? sortedCandidateCards : candidateCards} */}
          </Stack>
        </Stack>
        {lgUp && layout === "leftNav" && (
          <Box>
            <ReportsBarChart
              dataset={filteredData}
              timeRange={filters.timeRange}
              feeds={feeds}
            />
          </Box>
        )}
      </Box>
    </Container>
  );
}
