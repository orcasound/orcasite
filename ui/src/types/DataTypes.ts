import { DetectionsQuery } from "@/graphql/generated";

// Data Transfer Objects (DTOs) come from specific query results and may omit fields from full schema models.
// DetectionsResultList is a DTO alias so adapters match query output (listDetections) rather than requiring every field on the GraphQL `Detection` model type.

// NonNullable is a built-in TypeScript utility type that removes null and undefined from a type, ensuring that the resulting type is always defined.
type DetectionsResultList = NonNullable<
  NonNullable<DetectionsQuery["detections"]>["results"]
>;

// [number] means 'type of one item in the DetectionsQuery["detections"]>["results"] array'
export type DetectionsResult = DetectionsResultList[number];

export interface AudioDetection extends Omit<DetectionsResult, "candidate"> {
  type: "audio";
  standardizedFeedName: string;
  feedSlug: string;
  comments: string | null | undefined;
}

export interface CascadiaSighting {
  id: string;
  type: string; // e.g., "sighting"
  project_id: number;
  trip_id: number;
  name: string; // e.g., "Killer Whale (Orca)"
  scientific_name: string; // e.g., "Orcinus orca"
  number_sighted: number;
  latitude: number;
  longitude: number;
  created: string; // ISO date string, e.g., "2025-01-01 17:25:00"
  source: string; // e.g., "whale_alert"
  comments: string | null | undefined; // HTML string, null | undefined matches Orcahello
  icon: string; // e.g., "dot-black"
  photo_url: string;
  usernm: string; // e.g., "cascadiaWebMap"
  count_check: number;
  in_ocean: number; // boolean-like (0 or 1)
  is_test: number; // boolean-like (0 or 1)
  moderated: number | string; // boolean-like (0 or 1) for Cascadia, string for Orcahello
  trusted: number; // boolean-like (0 or 1)
}

export interface Sighting extends CascadiaSighting {
  type: "sightings";
  standardizedFeedName: string;
  feedSlug: string;
  feedId: string;
  timestamp: Date;
}

export interface AIDetectionLocation {
  name: string;
  longitude: number;
  latitude: number;
}

export interface AIDetectionAnnotation {
  id: number;
  startTime: number;
  endTime: number;
  confidence: number;
}

export type AIDetectionFound = "yes" | "no" | "don't know" | null;

export type AIDetectionReviewState =
  | "confirmed"
  | "falsepositive"
  | "unknown"
  | "unreviewed";

export interface AIDetectionRaw {
  id: string | null;
  audioUri: string | null;
  spectrogramUri: string | null;
  location: AIDetectionLocation;
  timestamp: string;
  annotations: AIDetectionAnnotation[] | null;
  reviewed: boolean;
  found: string | null;
  comments: string | null;
  confidence: number;
  moderator: string | null;
  moderated: string | null;
  tags: string | null;
}

export interface AIDetection
  extends Omit<
    AIDetectionRaw,
    "id" | "timestamp" | "annotations" | "found" | "tags"
  > {
  id: string;
  type: "ai";
  standardizedFeedName: string;
  feedSlug?: string;
  feedId?: string;
  comments: string | null;
  timestamp: Date;
  annotations: AIDetectionAnnotation[];
  found: AIDetectionFound;
  reviewState: AIDetectionReviewState;
  tags: string[];
}

export type CombinedData = AudioDetection | AIDetection | Sighting;

// future type for transformed data object
// export interface Candidate {
//   id: string;
//   array: CombinedData[];
//   startTimestamp: string;
//   endTimestamp: string;
//   whale: number;
//   vessel: number;
//   other: number;
//   "whale (AI)": number;
//   sightings: number;
//   hydrophone: string;
//   feedId: string | undefined;
//   clipCount: string;
//   descriptions: string;
// }
