import dayjs, { Dayjs } from "dayjs";
import React, { createContext, useContext, useEffect, useState } from "react";

import { defaultRange } from "@/components/CandidateList/CandidateListFilters";
import useFilteredData from "@/components/CandidateList/useFilteredData";
import useSortedCandidates from "@/components/CandidateList/useSortedCandidates";
import { Feed } from "@/graphql/generated";
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
  const { setQueue, nowPlaying, setNowPlaying } = useNowPlaying();
  useEffect(() => {
    if (nowPlaying === undefined || !Object.keys(nowPlaying).length) {
      setNowPlaying(sortedCandidates[0]);
    }
    console.log(JSON.stringify(sortedCandidates[14], null, 2));
    setQueue(sortedCandidates);
  }, [sortedCandidates, setQueue, nowPlaying, setNowPlaying]);

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
