import { useMemo } from "react";

import {
  Detection,
  Feed,
  useDetectionsQuery,
  useFeedsQuery,
} from "@/graphql/generated";
import { AIData, CombinedData, HumanData, Sighting } from "@/types/DataTypes";

import {
  transformAI,
  transformHuman,
  transformSightings,
} from "../utils/masterDataTransforms";
import { useAIDetections } from "./useAIDetections";
import { useLiveDetections } from "./useLiveDetections";
import { useLiveFeeds } from "./useLiveFeeds";
import { useSightings } from "./useSightings";

type MasterData = {
  human: HumanData[];
  ai: AIData[];
  sightings: Sighting[];
  combined: CombinedData[];
  feeds: Feed[];
  isSuccessOrcahello: boolean;
};

export function useMasterData(useLiveData: boolean): MasterData {
  //// ORCASOUND
  // get feeds and detections based on live/seed toggle in development UI
  const seedDetections = useDetectionsQuery().data?.detections?.results ?? [];
  const liveDetections = useLiveDetections().data?.detections?.results ?? [];
  const humanDetections = useLiveData
    ? liveDetections
    : (seedDetections as Detection[]);

  const liveFeeds = useLiveFeeds().data?.feeds ?? [];
  const seedFeeds = useFeedsQuery().data?.feeds ?? [];
  const feeds = useLiveData ? liveFeeds : (seedFeeds as Feed[]);

  // standardize data
  const datasetHuman = useMemo(
    () => transformHuman(humanDetections, feeds),
    [humanDetections, feeds],
  );

  //// ORCAHELLO
  // get detections
  const { data: aiDetections = [], isSuccess: aiSuccess } = useAIDetections();

  // standardize data
  const datasetAI = useMemo(
    () => transformAI(aiDetections, feeds),
    [aiDetections, feeds],
  );

  //// CASCADIA.IO
  // get detections
  const { data: sightingsData, isSuccess: isSuccessSightings } = useSightings();
  const dataSightings = useMemo(
    () => sightingsData?.results ?? [],
    [sightingsData],
  );
  // standardize data
  const datasetSightings = useMemo(
    () => transformSightings(dataSightings, feeds),
    [dataSightings, feeds],
  );

  const combined: CombinedData[] = useMemo(() => {
    return [...datasetHuman, ...datasetAI, ...datasetSightings];
  }, [datasetHuman, datasetAI, datasetSightings]);

  const dataset = useMemo(() => {
    return {
      human: datasetHuman,
      ai: datasetAI,
      sightings: datasetSightings,
      combined: combined,
      feeds: feeds,
      isSuccessOrcahello: aiSuccess,
      isSuccessSightings: isSuccessSightings,
    };
  }, [
    datasetHuman,
    datasetAI,
    datasetSightings,
    combined,
    feeds,
    aiSuccess,
    isSuccessSightings,
  ]);
  return dataset;
}
