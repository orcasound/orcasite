import dayjs, { Dayjs } from "dayjs";
import { useRouter } from "next/router";
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

interface DataContextType {
  feeds: Feed[];
  filteredData: CombinedData[];
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
    masterPlayerRef,
  } = useNowPlaying();

  // on first load, if there isn't a feed or candidate selected
  const autoPlayOnReady = useRef(true);
  const router = useRouter();

  const tabSlugs = ["hydrophones", "candidates", "visualizations"];
  const slug = router.route.replace("/beta/", "");

  function getTabIndexFromPath(path: string): number {
    const slug = path.replace("/beta/", "");
    if (tabSlugs.includes(slug)) {
      return tabSlugs.indexOf(slug);
    } else {
      return 0;
    }
  }

  // useEffect(() => {
  //   if (!nowPlayingCandidate && !nowPlayingFeed) {
  //     autoPlayOnReady.current = false;
  //     setNowPlayingCandidate(sortedCandidates[0]);
  //   }

  //   if (setQueue) setQueue(sortedCandidates);
  // }, [
  //   sortedCandidates,
  //   setQueue,
  //   nowPlayingCandidate,
  //   setNowPlayingCandidate,
  //   nowPlayingFeed,
  // ]);

  return (
    <DataContext.Provider
      value={{
        feeds,
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
