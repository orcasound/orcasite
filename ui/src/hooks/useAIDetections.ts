import { useQuery } from "@tanstack/react-query";

import { AIDetectionRaw } from "@/types/DataTypes";
import { constructUrl } from "@/utils/dataHelpers";

const endpointOrcahello =
  "https://aifororcasdetections.azurewebsites.net/api/detections";

const RECORDS_PER_PAGE = 50;
const MAX_PAGES = 100;

export type AIDetectionsMetaData = {
  requestedStart: string | null;
  requestedEnd: string | null;
  fetchedNewest: string | null;
  fetchedOldest: string | null;
  failedPage: number | null;
  loadError: string | null;
  partial: boolean;
};

export type AIDetectionTimeframe =
  | "30m"
  | "3h"
  | "6h"
  | "24h"
  | "1w"
  | "1m"
  | "range"
  | "all";

export type AIDetectionsOptions = {
  timeframe?: AIDetectionTimeframe;
  startDate?: string;
  endDate?: string;
  location?: string;
  enabled?: boolean;
};

// fetches a single page of results (50 max)
const fetchOrcahelloPage = async (
  params: Record<string, string | number>,
  page: number,
): Promise<AIDetectionRaw[]> => {
  const url = constructUrl(endpointOrcahello, {
    ...params,
    page,
  });
  const response = await fetch(url);

  if (!response.ok) {
    throw new Error(`Orcahello request failed with ${response.status}`);
  }

  const rows = (await response.json()) as AIDetectionRaw[];
  return rows;
};

// creates metadata object
const buildMetaData = (
  rows: AIDetectionRaw[],
  requestedStart: string | null,
  requestedEnd: string | null,
  failedPage: number | null,
  loadError: string | null,
): AIDetectionsMetaData => ({
  requestedStart,
  requestedEnd,
  fetchedNewest: rows[0]?.timestamp ?? null,
  fetchedOldest: rows[rows.length - 1]?.timestamp ?? null,
  failedPage,
  loadError,
  partial: failedPage !== null,
});

// loops over all data pages until it errors or no more records
const fetchOrcahelloData = async ({
  timeframe,
  startDate,
  endDate,
  location,
}: AIDetectionsOptions): Promise<{
  detections: AIDetectionRaw[];
  meta: AIDetectionsMetaData;
}> => {
  const params: Record<string, string | number> = {
    sortBy: "timestamp",
    sortOrder: "desc",
    timeframe: timeframe ?? "1w",
    location: location ?? "all",
    recordsPerPage: RECORDS_PER_PAGE,
  };

  if (timeframe === "range") {
    if (!startDate || !endDate) {
      throw new Error(
        "useAIDetections requires startDate and endDate when timeframe is 'range'",
      );
    }
    params.dateFrom = startDate;
    params.dateTo = endDate;
  }

  const allRows: AIDetectionRaw[] = [];

  for (let page = 1; page <= MAX_PAGES; page += 1) {
    try {
      const nextPage = await fetchOrcahelloPage(params, page);

      if (nextPage.length === 0) {
        break;
      }

      allRows.push(...nextPage);

      if (nextPage.length < RECORDS_PER_PAGE) {
        break;
      }
    } catch (error) {
      const message =
        error instanceof Error ? error.message : "Unknown Orcahello error";
      return {
        detections: allRows,
        meta: buildMetaData(
          allRows,
          startDate ?? null,
          endDate ?? null,
          page,
          message,
        ),
      };
    }
  }

  return {
    detections: allRows,
    meta: buildMetaData(
      allRows,
      startDate ?? null,
      endDate ?? null,
      null,
      null,
    ),
  };
};

export function useAIDetections(options: AIDetectionsOptions = {}) {
  const timeframe = options.timeframe ?? "1w";
  const startDate = timeframe === "range" ? options.startDate : undefined;
  const endDate = timeframe === "range" ? options.endDate : undefined;
  const location = options.location ?? "all";
  const enabled = options.enabled ?? true;

  const { data, isSuccess, isFetching, isPending, error } = useQuery({
    queryKey: ["ai-detections", { timeframe, startDate, endDate, location }],
    queryFn: () =>
      fetchOrcahelloData({
        timeframe,
        startDate,
        endDate,
        location,
      }),
    staleTime: 5 * 60 * 1000,
    refetchOnWindowFocus: false,
    enabled,
  });

  const aiDetections = data?.detections;
  const metaData = data?.meta;

  return {
    aiDetections,
    metaData,
    isSuccess,
    isFetching,
    isPending,
    error,
  };
}
