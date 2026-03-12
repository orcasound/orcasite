import { useMemo } from "react";

import { Feed, useDetectionsQuery, useFeedsQuery } from "@/graphql/generated";
import {
  AIDetection,
  AudioDetection,
  CombinedData,
  Sighting,
} from "@/types/DataTypes";
import {
  transformAIDetection,
  transformAudioDetections,
  transformSightings,
} from "@/utils/dataTransforms";

import { useAIDetections } from "./useAIDetections";
import { useSightings } from "./useSightings";

type CombinedDataResult = {
  audio: AudioDetection[];
  ai: AIDetection[];
  sightings: Sighting[];
  combined: CombinedData[];
  feeds: Feed[];
};

export function useCombinedData(): CombinedDataResult {
  //// ORCASOUND
  const detectionsResults = useDetectionsQuery().data?.detections?.results;
  const audioDetections = useMemo(
    () => detectionsResults ?? [],
    [detectionsResults],
  );

  const seedFeeds = useFeedsQuery().data?.feeds ?? ([] as Feed[]);
  const feeds = seedFeeds as Feed[];

  //// Transform ORCAHELLO detections
  const { aiDetections: aiDetectionsRaw = [], isSuccess: isSuccessOrcahello } =
    useAIDetections();
  const aiDetections = aiDetectionsRaw.map((d) => {
    return transformAIDetection(d, feeds);
  });

  // Use machine detections from Orcasound API until more detailed Orcahello data comes through
  const datasetAudio = useMemo(() => {
    if (!isSuccessOrcahello) {
      return transformAudioDetections(audioDetections, feeds);
    } else {
      return transformAudioDetections(
        audioDetections.filter((d) => d.source === "HUMAN"),
        feeds,
      );
    }
  }, [audioDetections, feeds, isSuccessOrcahello]);

  //// Filter out categories of Orcahello detections
  const unreviewed = aiDetections?.filter((el: AIDetection) => !el.reviewed);
  const confirmed = aiDetections?.filter(
    (el: AIDetection) => el.reviewed && el.found === "yes",
  );
  const falsepositives = aiDetections?.filter(
    (el: AIDetection) => el.reviewed && el.found === "no",
  );
  const unknown = aiDetections?.filter(
    (el: AIDetection) => el.reviewed && el.found === "don't know",
  );

  //// Transform ACARTIA sightings
  const sightingResults = useSightings().data?.results;
  const sightings = useMemo(
    () => transformSightings(sightingResults, feeds),
    [sightingResults, feeds],
  );

  //// COMBINED all three
  const combined: CombinedData[] = useMemo(() => {
    return [...datasetAudio, ...sightings, ...aiDetections];
  }, [datasetAudio, sightings, aiDetections]);

  // Construct result object
  const dataset: CombinedDataResult = useMemo(() => {
    return {
      audio: datasetAudio,
      sightings: sightings,
      ai: aiDetections,
      unreviewed: unreviewed,
      confirmed: confirmed,
      falsepositives: falsepositives,
      unknown: unknown,
      combined: combined,
      feeds: feeds,
      isSuccessSightings: !!sightingResults,
      isSuccessOrcahello: !!aiDetections,
    };
  }, [datasetAudio, sightings, combined, feeds, sightingResults, aiDetections]);

  return dataset;
}
