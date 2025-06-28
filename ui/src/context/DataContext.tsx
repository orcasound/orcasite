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
import useFilteredData from "@/hooks/beta/useFilteredData";
import { useSortedCandidates } from "@/hooks/beta/useSortedCandidates";
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
  detectionsGreaterThan: number;
}

type FeedCountData = {
  counts: Record<string, number>;
  countString: string;
  shortCountString: string;
};

type FeedCounts = {
  [feedId: string]: FeedCountData;
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
  lastWhaleReport: (feed?: Feed | null) => CombinedData | undefined;
  lastWhaleReportFeed: Feed | null;
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
    detectionsGreaterThan: 1,
  });

  const filteredData = useFilteredData(data.combined, filters);

  const lastWhaleReport = (feed?: Feed | null) => {
    const reports = feed
      ? filteredData.filter((d) => d.feedId === feed?.id)
      : filteredData;
    const whaleReports = reports.filter(
      (d) =>
        d.newCategory === "WHALE" ||
        d.newCategory === "WHALE (AI)" ||
        d.newCategory === "SIGHTING",
    );
    const lastReportTime = Math.max(
      ...whaleReports.map((r) => new Date(r.timestampString).getTime()),
    );
    const lastReport = whaleReports.find(
      (r) => new Date(r.timestampString).getTime() === lastReportTime,
    );
    return lastReport;
  };

  const lastWhaleReportFeed =
    feeds.find((f) => f.id === lastWhaleReport()?.feedId) ?? null;

  // if there is no feed or candidate selected, auto-select the feed with the most recent whale report in the time range, or the first feed in the list if nothing else
  // useEffect(() => {
  //   if (lastWhaleReportFeed && !nowPlayingCandidate && !nowPlayingFeed) {
  //     setNowPlayingFeed(lastWhaleReportFeed);
  //   } else if (!lastWhaleReportFeed) {
  //     setNowPlayingFeed(feeds[0]);
  //   }
  // });

  const sortedCandidates = useSortedCandidates(
    filteredData,
    filters.timeIncrement,
    filters.sortOrder,
    filters.detectionsGreaterThan,
  );

  const reportCounter = (
    startTimestamp?: string | null,
    endTimestamp?: string | null,
  ) => {
    const categories = [...new Set(filteredData.map((el) => el.newCategory))];

    const obj: FeedCounts = {
      all: { counts: {}, countString: "", shortCountString: "" },
    };

    feeds.forEach((feed) => {
      if (feed.id != null) {
        obj[feed.id] = { counts: {}, countString: "", shortCountString: "" };
      }
    });

    for (const feedId in obj) {
      categories.forEach((c) => {
        obj[feedId]["counts"][c] = 0;
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

    const shortCountString = (obj: Record<string, number>) => {
      const stringParts: string[] = [];
      const shortCount = {
        whale: 0,
        vessel: 0,
        other: 0,
      };
      for (const key of Object.keys(obj)) {
        if (key === "WHALE" || key === "WHALE (AI)" || key === "SIGHTING") {
          shortCount.whale += obj[key];
        } else if (key === "VESSEL") {
          shortCount.vessel += obj[key];
        } else if (key === "OTHER") {
          shortCount.other += obj[key];
        }
      }
      Object.entries(shortCount).forEach(([key, count]) => {
        stringParts.push(`${count} ${key}`);
      });
      return stringParts.join(" · ");
    };
    for (const feed in obj) {
      obj[feed].countString = countString(obj[feed]["counts"]);
      obj[feed].shortCountString = shortCountString(obj[feed]["counts"]);
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
        lastWhaleReport,
        lastWhaleReportFeed,
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
