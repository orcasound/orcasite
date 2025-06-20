import { useQuery } from "@tanstack/react-query";
import { useEffect, useMemo, useState } from "react";

import { AIData } from "@/types/DataTypes";
import { apiTodayUTC, constructUrl } from "@/utils/masterDataHelpers";

const endpointOrcahello =
  "https://aifororcasdetections.azurewebsites.net/api/detections";
const startDateOrcahello = "2025-01-01";
const paramsOrcahello = {
  page: 1,
  sortBy: "timestamp",
  sortOrder: "desc",
  timeframe: "all",
  dateFrom: startDateOrcahello,
  dateTo: apiTodayUTC,
  location: "all",
  recordsPerPage: 100,
};

const fetchOrcahelloData = async () => {
  const response = await fetch(
    constructUrl(endpointOrcahello, paramsOrcahello),
  );
  if (!response.ok) {
    throw new Error("Network response from Orcahello was not ok");
  }
  return response.json();
};

export function useAIDetections() {
  const [cached, setCached] = useState<AIData[] | null>(null);

  useEffect(() => {
    const raw = localStorage.getItem("orcahello-ai-detections");
    if (raw) setCached(JSON.parse(raw));
  }, []);

  const { data, isSuccess } = useQuery({
    queryKey: ["ai-detections"],
    queryFn: fetchOrcahelloData,
  });

  const truePositives = data?.filter((el: AIData) => el.found === "yes");

  useEffect(() => {
    if (isSuccess && truePositives) {
      localStorage.setItem(
        "orcahello-ai-detections",
        JSON.stringify(truePositives),
      );
    }
  }, [isSuccess, truePositives]);

  const returnData = useMemo(() => {
    return truePositives ?? cached;
  }, [truePositives, cached]);

  return { data: returnData, isSuccess: isSuccess };
}
