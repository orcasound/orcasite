import { useQuery } from "@tanstack/react-query";

import { CascadiaSighting } from "@/types/DataTypes";
import { apiTodayUTC, constructUrl } from "@/utils/masterDataHelpers";

const endpointCascadia =
  "https://maplify.com/waseak/php/search-all-sightings.php";

const startDateCascadia = "2025-01-01";
const paramsCascadia = {
  BBOX: "-136,36,-120,54",
  start: startDateCascadia,
  end: apiTodayUTC,
};

// live data call for Detections
type SightingsDataResponse = {
  results: CascadiaSighting[];
};

const fetchCascadiaData = async (): Promise<SightingsDataResponse> => {
  const response = await fetch(constructUrl(endpointCascadia, paramsCascadia));
  if (!response.ok) {
    throw new Error("Network response from Orcahello was not ok");
  }
  return response.json();
};

export function useSightings() {
  const { data, isSuccess, error } = useQuery({
    queryKey: ["sightings"],
    queryFn: fetchCascadiaData,
  });
  return { data, isSuccess, error };
}
