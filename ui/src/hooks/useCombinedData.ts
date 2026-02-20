import { useMemo } from "react";

import { Feed, useDetectionsQuery, useFeedsQuery } from "@/graphql/generated";
import { AudioDetection, CombinedData, Sighting } from "@/types/DataTypes";
import {
  transformAudioDetections,
  transformSightings,
} from "@/utils/dataTransforms";

import { useSightings } from "./useSightings";

type CombinedDataObject = {
  audio: AudioDetection[];
  sightings: Sighting[];
  combined: CombinedData[];
  feeds: Feed[];
};

export function useCombinedData(): CombinedDataObject {
  //// ORCASOUND
  // get feeds and detections based on live/seed toggle in development UI
  const detectionsResults = useDetectionsQuery().data?.detections?.results;
  const audioDetections = useMemo(
    () => detectionsResults ?? [],
    [detectionsResults],
  );

  const seedFeeds = useFeedsQuery().data?.feeds ?? ([] as Feed[]);
  const feeds = seedFeeds as Feed[];

  // standardize data
  const datasetAudio = useMemo(
    () => transformAudioDetections(audioDetections, feeds),
    [audioDetections, feeds],
  );

  //// ACARTIA sightings
  // get detections
  const sightingResults = useSightings().data?.results;
  // standardize data
  const sightings = useMemo(
    () => transformSightings(sightingResults, feeds),
    [sightingResults, feeds],
  );

  const combined: CombinedData[] = useMemo(() => {
    return [...datasetAudio, ...sightings];
  }, [datasetAudio, sightings]);

  const dataset = useMemo(() => {
    return {
      audio: datasetAudio,
      sightings: sightings,
      combined: combined,
      feeds: feeds,
      isSuccessSightings: !!sightingResults,
    };
  }, [datasetAudio, sightings, combined, feeds, sightingResults]);
  return dataset;
}
