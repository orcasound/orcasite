import {
  Box,
  Card,
  CardActionArea,
  CardContent,
  Stack,
  Typography,
} from "@mui/material";
import { SelectChangeEvent } from "@mui/material/Select";
import React from "react";

import ChartSelect from "@/components/ChartSelect";
import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import ReportsBarChart from "@/components/ReportsBarChart";
import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";
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

interface Data {
  category: string;
  description: string;
  id: string;
  timestamp: string;
  comments: string;
  listenerCount: number;
  feed: FeedName;
  hydrophone: string;
}
interface FeedName {
  name: string;
}

const createCandidates = (dataset: CombinedData[], interval: number) => {
  const candidates: Array<Array<CombinedData>> = [];
  dataset.forEach((el: CombinedData) => {
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
        lastMatchingArray[lastMatchingArray.length - 1].dateString;
      if (
        lastTimestamp &&
        Math.abs(Date.parse(lastTimestamp) - Date.parse(el.dateString)) /
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

interface Candidate {
  array: Array<CombinedData>;
  whale: number;
  vessel: number;
  other: number;
  "whale (AI)": number;
  hydrophone: string;
  descriptions: string;
}

export default function Candidates() {
  const {
    combined,
    feeds,
  }: { combined: CombinedData[] | undefined; feeds: Feed[] } = useData(); // this uses a context provider to call data once and make it available to all children -- this may not be better than just using the query hooks, kind of does the same thing
  const [filters, setFilters] = React.useState({
    timeRange: threeDays,
    timeIncrement: 15,
    hydrophone: "All hydrophones",
    category: "All categories",
  });

  const handleChange = (event: SelectChangeEvent<HTMLSelectElement>) => {
    const { name, value } = event.target;
    setFilters((prevFilters) => ({
      ...prevFilters,
      [name]: value,
    }));
  };

  const feedList = feeds.map((el) => ({
    label: el.name,
    value: el.name,
  }));
  feedList.unshift({ label: "All hydrophones", value: "All hydrophones" });

  const filteredData = combined.filter((el: CombinedData) => {
    return (
      // Disabling timerange filter for now because seed data is all from 2023
      //            (Date.parse(el.timestamp) >= min)
      //            &&
      (filters.hydrophone === "All hydrophones" ||
        el.hydrophone === filters.hydrophone) &&
      (filters.category === "All categories" ||
        el.newCategory.toLowerCase() === filters.category)
    );
  });

  const candidates = createCandidates(filteredData, filters.timeIncrement);

  return (
    <Stack>
      <ReportsBarChart
        dataset={filteredData}
        timeRange={filters.timeRange}
        feedList={feeds}
      />
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
      <Stack spacing={3}>
        {/*<pre>{JSON.stringify(candidates, null, 2)}</pre>*/}

        {candidates.map((candidate: Candidate) => (
          <Card key={candidate.array[0].dateString}>
            <CardActionArea>
              <CardContent>
                <Typography variant="h6" component="div">
                  {new Date(
                    candidate.array[candidate.array.length - 1].timestamp,
                  ).toLocaleString()}
                </Typography>
                <Typography variant="body1">
                  {candidate.hydrophone}
                  {" • "}
                  {!Math.round(
                    (Date.parse(candidate.array[0].dateString) -
                      Date.parse(
                        candidate.array[candidate.array.length - 1].dateString,
                      )) /
                      (1000 * 60),
                  )
                    ? "30 seconds"
                    : Math.round(
                          (Date.parse(candidate.array[0].dateString) -
                            Date.parse(
                              candidate.array[candidate.array.length - 1]
                                .dateString,
                            )) /
                            (1000 * 60),
                        ) >= 1
                      ? Math.round(
                          (Date.parse(candidate.array[0].dateString) -
                            Date.parse(
                              candidate.array[candidate.array.length - 1]
                                .dateString,
                            )) /
                            (1000 * 60),
                        ) + " minutes"
                      : Math.round(
                          (Date.parse(candidate.array[0].dateString) -
                            Date.parse(
                              candidate.array[candidate.array.length - 1]
                                .dateString,
                            )) /
                            (1000 * 60 * 60),
                        ) + " seconds"}
                  <br />
                  {["whale", "vessel", "other", "whale (AI)"]
                    .map((item) =>
                      candidate[item as keyof Candidate]
                        ? candidate[item as keyof Candidate] + "  " + item
                        : null,
                    )
                    .filter((candidate) => candidate !== null)
                    .join(" • ")}
                  <br />
                  {candidate.descriptions ? (
                    <span>{"Descriptions: " + candidate.descriptions}</span>
                  ) : (
                    <br />
                  )}
                </Typography>
              </CardContent>
            </CardActionArea>
          </Card>
        ))}
      </Stack>
    </Stack>
  );
}

Candidates.getLayout = getModeratorLayout;
