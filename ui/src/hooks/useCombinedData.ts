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

export function useCombinedData(useLiveData: boolean): CombinedDataObject {
  //// ORCASOUND
  // get feeds and detections based on live/seed toggle in development UI
  const audioDetections = useDetectionsQuery().data?.detections?.results ?? [];

  const seedFeeds = useFeedsQuery().data?.feeds ?? ([] as Feed[]);
  const feeds = seedFeeds as Feed[];

  // standardize data
  const datasetAudio = useMemo(
    () => transformAudioDetections(audioDetections, feeds),
    [audioDetections, feeds],
  );

  //// ACARTIA sightings
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
    return [...datasetAudio, ...datasetSightings];
  }, [datasetAudio, datasetSightings]);

  const dataset = useMemo(() => {
    return {
      audio: datasetAudio,
      sightings: datasetSightings,
      combined: combined,
      feeds: feeds,
      isSuccessSightings: isSuccessSightings,
    };
  }, [datasetAudio, datasetSightings, combined, feeds, isSuccessSightings]);
  return dataset;
}
