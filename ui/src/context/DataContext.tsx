import dayjs, { Dayjs } from "dayjs";
import React, { createContext, useContext, useEffect, useState } from "react";

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

interface DataContextType {
  feeds: Feed[];
  filteredData: CombinedData[];
  sortedCandidates: Candidate[];
  filters: CandidateFilters;
  setFilters: React.Dispatch<React.SetStateAction<CandidateFilters>>;
  isSuccessOrcahello: boolean;
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

  // playbar queue
  const {
    setQueue,
    nowPlayingCandidate,
    setNowPlayingCandidate,
    nowPlayingFeed,
  } = useNowPlaying();
  useEffect(() => {
    if (!nowPlayingCandidate && !nowPlayingFeed) {
      setNowPlayingCandidate(sortedCandidates[0]);
    }
    if (setQueue) setQueue(sortedCandidates);
  }, [
    sortedCandidates,
    setQueue,
    nowPlayingCandidate,
    setNowPlayingCandidate,
    nowPlayingFeed,
  ]);

  return (
    <DataContext.Provider
      value={{
        feeds,
        filteredData,
        sortedCandidates,
        filters,
        setFilters,
        isSuccessOrcahello,
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
