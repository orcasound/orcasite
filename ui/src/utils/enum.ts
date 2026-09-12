import { AudioCategory, DetectionCategory } from "@/graphql/generated";

export function detectionCategoryToAudioCategory(
  detectionCategory: DetectionCategory,
): AudioCategory {
  return {
    WHALE: AudioCategory.Biophony,
    VESSEL: AudioCategory.Anthrophony,
    OTHER: AudioCategory.Geophony,
  }[detectionCategory];
}

export function audioCategoryToDetectionCategory(
  audioCategory: AudioCategory,
): DetectionCategory {
  return {
    BIOPHONY: DetectionCategory.Whale,
    ANTHROPHONY: DetectionCategory.Vessel,
    GEOPHONY: DetectionCategory.Other,
  }[audioCategory];
}
