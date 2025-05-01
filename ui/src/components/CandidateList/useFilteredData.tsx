import dayjs from "dayjs";
import { useMemo } from "react";

import { CombinedData } from "@/types/DataTypes";

import { allTime, customRange } from "./CandidateListFilters";
import { CandidateFilters } from "./CandidatesList";

export default function useFilteredData(
  data: CombinedData[],
  filters: CandidateFilters,
  searchQuery: string,
): CombinedData[] {
  return useMemo(() => {
    const min = Date.now() - filters.timeRange;
    console.log("filteredData recalculated");

    return data.filter((el: CombinedData) => {
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
  }, [data, filters, searchQuery]);
}
