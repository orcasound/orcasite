import { useQuery } from "@tanstack/react-query";

import { CascadiaSighting } from "@/types/DataTypes";
import { apiTodayUTC, constructUrl } from "@/utils/dataHelpers";
import { getDateMsAgo, rangeOptions } from "@/utils/dataHelpers";

type SightingsDataResponse = {
  results: CascadiaSighting[];
};

export function useSightings(startDate?: string, endDate?: string) {
  const endpoint = "https://maplify.com/waseak/php/search-all-sightings.php";

  if (startDate === undefined)
    startDate = getDateMsAgo(rangeOptions.sevenDays)
      .toISOString()
      .split("T")[0]; // e.g. "2025-01-01"
  if (endDate === undefined) endDate = apiTodayUTC;

  const params = {
    BBOX: "-136,36,-120,54",
    start: startDate,
    end: endDate,
  };

  const fetchSightings = async (): Promise<SightingsDataResponse> => {
    const response = await fetch(constructUrl(endpoint, params));
    if (!response.ok) {
      throw new Error("Network response from URL was not ok");
    }
    return response.json();
  };

  const { data, isSuccess, error } = useQuery({
    queryKey: ["sightings"],
    queryFn: fetchSightings,
  });
  return { data, isSuccess, error };
}
