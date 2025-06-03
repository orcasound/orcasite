import dayjs, { Dayjs } from "dayjs";
import React, {
  createContext,
  MutableRefObject,
  useContext,
  useRef,
  useState,
} from "react";

import { defaultRange } from "@/components/CandidateList/CandidateListFilters";
import { Feed } from "@/graphql/generated";
import useFilteredData from "@/hooks/useFilteredData";
import { useSortedCandidates } from "@/hooks/useSortedCandidates";
import { Candidate, CombinedData, Dataset } from "@/types/DataTypes";

import { useNowPlaying } from "./NowPlayingContext";

// filters
export interface CandidateFilters {
  timeRange: number;
  timeIncrement: number;
  hydrophone: string;
  category: string;
  startDate: Dayjs | null;
  endDate: Dayjs | null;
  sortOrder: "asc" | "desc";
  searchQuery: string;
  chartLegend: "category" | "hydrophone";
}

type FeedCountData = {
  counts: Record<string, number>;
  countString: string;
};

type FeedCounts = {
  [feed: string]: FeedCountData;
};
interface DataContextType {
  feeds: Feed[];
  filteredData: CombinedData[];
  reportCount: FeedCounts;
  sortedCandidates: Candidate[];
  filters: CandidateFilters;
  setFilters: React.Dispatch<React.SetStateAction<CandidateFilters>>;
  isSuccessOrcahello: boolean;
  autoPlayOnReady: MutableRefObject<boolean>;
}

const DataContext = createContext<DataContextType | undefined>(undefined);

export const DataProvider = ({
  children,
  data,
}: {
  children: React.ReactNode;
  data: Dataset;
}) => {
  const feeds = data.feeds;
  const isSuccessOrcahello = data.isSuccessOrcahello;
  const { nowPlayingCandidate } = useNowPlaying();

  const [filters, setFilters] = useState<CandidateFilters>({
    timeRange: defaultRange,
    timeIncrement: 15,
    hydrophone: "All hydrophones",
    category: "All categories",
    startDate: null,
    endDate: dayjs(),
    sortOrder: "desc",
    searchQuery: "",
    chartLegend: "category",
  });

  const filteredData = useFilteredData(data.combined, filters);

  const sortedCandidates = useSortedCandidates(
    filteredData,
    filters.timeIncrement,
    filters.sortOrder,
  );

  const reportCounter = (
    startTimestamp?: string | null,
    endTimestamp?: string | null,
  ) => {
    const categories = [...new Set(filteredData.map((el) => el.newCategory))];

    const obj: FeedCounts = { all: { counts: {}, countString: "" } };

    feeds.forEach((feed) => {
      if (feed.id != null) {
        obj[feed.id] = { counts: {}, countString: "" };
      }
    });

    for (const feed in obj) {
      categories.forEach((c) => {
        obj[feed]["counts"][c] = 0;
      });
    }

    const data =
      !startTimestamp || !endTimestamp
        ? filteredData
        : filteredData.filter((d) => {
            return (
              new Date(d.timestampString) >= new Date(startTimestamp) &&
              new Date(d.timestampString) <= new Date(endTimestamp)
            );
          });

    data.forEach((d) => {
      if (
        d.feedId &&
        d.newCategory &&
        obj[d.feedId] &&
        obj[d.feedId]["counts"][d.newCategory] != null
      ) {
        obj.all["counts"][d.newCategory] += 1;
        obj[d.feedId]["counts"][d.newCategory] += 1;
      }
    });

    const countString = (obj: Record<string, number>) => {
      const stringParts = [];
      for (const key of Object.keys(obj)) {
        if (typeof obj[key] === "number" && obj[key] > 0) {
          stringParts.push(
            `${obj[key]} ${key.toLowerCase()}${key === "SIGHTING" ? "s" : ""}`,
          );
        }
      }
      return stringParts.join(" · ");
    };

    for (const feed in obj) {
      obj[feed].countString = countString(obj[feed]["counts"]);
    }

    return obj;
  };

  const reportCount = nowPlayingCandidate
    ? reportCounter(
        nowPlayingCandidate.startTimestamp,
        nowPlayingCandidate.endTimestamp,
      )
    : reportCounter();

  // controls if player starts when it is loaded -- initial page load sets to false
  const autoPlayOnReady = useRef(true);

  return (
    <DataContext.Provider
      value={{
        feeds,
        reportCount,
        filteredData,
        sortedCandidates,
        filters,
        setFilters,
        isSuccessOrcahello,
        autoPlayOnReady,
      }}
    >
      {children}
    </DataContext.Provider>
  );
};

export const useData = (): DataContextType => {
  const context = useContext(DataContext);
  if (!context) {
    throw new Error("useData must be used within a DataContextProvider");
  }
  return context;
};
