import { Box, Button, Container, Stack, Typography } from "@mui/material";
import { SelectChangeEvent } from "@mui/material/Select";
import { useEffect, useRef, useState } from "react";

import CandidateCard from "@/components/CandidateCard";
import ChartSelect from "@/components/ChartSelect";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout";
import { useData } from "@/context/DataContext";
import { useFeedsQuery } from "@/graphql/generated";
import { CombinedData } from "@/types/DataTypes";

const sevenDays = 7 * 24 * 60 * 60 * 1000;
const threeDays = 3 * 24 * 60 * 60 * 1000;
const oneDay = 24 * 60 * 60 * 1000;

const timeRangeSelect = [
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
];

export interface Candidate {
  array: CombinedData[];
  whale: number;
  vessel: number;
  other: number;
  "whale (AI)": number;
  hydrophone: string;
  descriptions: string;
}

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

export default function Candidates() {
  // replace this with a direct react-query...
  const {
    combined,
    isSuccess,
  }: { combined: CombinedData[] | undefined; isSuccess: boolean } = useData(); // this uses a context provider to call data once and make it available to all children -- this may not be better than just using the query hooks, kind of does the same thing

  // get hydrophone feed list
  const feedsQueryResult = useFeedsQuery();
  const feeds = feedsQueryResult.data?.feeds ?? [];

  const [filters, setFilters] = useState({
    timeRange: threeDays,
    timeIncrement: 15,
    hydrophone: "All hydrophones",
    category: "All categories",
  });

  const [timeRange, setTimeRange] = useState(threeDays);
  const [timeIncrement, setTimeIncrement] = useState(15);
  const [hydrophone, setHydrophone] = useState("All hydrophones");
  const [category, setCategory] = useState("All categories");

  const handleChange = (event: SelectChangeEvent<unknown>) => {
    const { name, value } = event.target;
    setFilters((prevFilters) => ({
      ...prevFilters,
      [name]: value,
    }));
  };

  const initChartSelect = (name: string, value: string | number) => {
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

  const [playNext, setPlayNext] = useState(true);

  const players = useRef({});

  const feedList = feeds.map((el) => ({
    label: el.name,
    value: el.name,
  }));
  feedList.unshift({ label: "All hydrophones", value: "All hydrophones" });

  const filteredData = combined.filter((el: CombinedData) => {
    return (
      // uncomment this to block Orcahello data
      // el.type === "human" &&

      // Disabling timerange filter for now because seed data is all from 2023
      //            (Date.parse(el.timestamp) >= min) &&

      (filters.hydrophone === "All hydrophones" ||
        el.hydrophone === filters.hydrophone) &&
      (filters.category === "All categories" ||
        el.newCategory.toLowerCase() === filters.category)
    );
  });

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

  const candidates = sortDescending(
    createCandidates(filteredData, filters.timeIncrement),
  );
  const [sortedCandidates, setSortedCandidates] = useState([...candidates]);

  const handleSortAscending = (array: Candidate[]) => {
    setSortedCandidates((v) => [...sortAscending(array)]);
  };

  const handleSortDescending = (array: Candidate[]) => {
    setSortedCandidates((v) => [...sortDescending(array)]);
  };

  useEffect(() => {
    setSortedCandidates((v) => [...candidates]);
    if (isSuccess) {
      setSortedCandidates((v) => [...candidates]);
    }
  }, [isSuccess]);

  // render these first because it seems to take a while for candidates to populate from state, could just be the dev environment
  const candidateCards = candidates.map(
    (candidate: Candidate, index: number) => (
      <CandidateCard
        candidate={candidate}
        key={index}
        index={index}
        // changeListState={changeListState}
        // command={playing.index === index ? "play" : "pause"}
        players={players}
        playNext={playNext}
      />
    ),
  );

  // these render from state after delay, then re-render after another delay when AI candidates come through
  const sortedCandidateCards = sortedCandidates.map(
    (candidate: Candidate, index: number) => (
      <CandidateCard
        candidate={candidate}
        key={index}
        index={index}
        // changeListState={changeListState}
        // command={playing.index === index ? "play" : "pause"}
        players={players}
        playNext={playNext}
      />
    ),
  );

  return (
    <Container>
      <Stack>
        <Box sx={{ paddingTop: 2 }}>
          <Typography variant="h5">Recordings</Typography>
        </Box>

        {/* <ReportsBarChart dataset={filteredData} timeRange={filters.timeRange} /> */}
        <Box
          style={{
            display: "flex",
            margin: "24px 0",
            gap: "1rem",
            flexWrap: "wrap",
          }}
        >
          <ChartSelect
            name={"timeRange"}
            value={filters.timeRange}
            list={timeRangeSelect}
            onChange={handleChange}
          />
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
        <Box sx={{ display: "flex", justifyContent: "space-between" }}>
          <Typography>
            Showing{" "}
            {sortedCandidates.length
              ? sortedCandidates.length
              : candidates.length}{" "}
            {!isSuccess
              ? "results from Orcasound, loading Orcahello..."
              : "results"}
          </Typography>
          <Box>
            <Button
              variant="outlined"
              onClick={() =>
                handleSortAscending(
                  sortedCandidates.length ? sortedCandidates : candidates,
                )
              }
            >
              Sort ascending
            </Button>
            <Button
              variant="outlined"
              onClick={() =>
                handleSortDescending(
                  sortedCandidates.length ? sortedCandidates : candidates,
                )
              }
            >
              Sort descending
            </Button>
          </Box>
        </Box>
        <Stack spacing={3}>
          {sortedCandidates.length ? sortedCandidateCards : candidateCards}
        </Stack>
      </Stack>
    </Container>
  );
}

Candidates.getLayout = getHalfMapLayout;
