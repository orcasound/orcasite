import { Dispatch, SetStateAction } from "react";

import { Detection, Feed, Scalars } from "@/graphql/generated";

interface HumanData extends Omit<Detection, "candidate"> {
  type: string;
  hydrophone: string;
  comments: string | null | undefined;
  newCategory: string;
  timestampString: string;
}

interface AIDetection {
  id: string;
  audioUri: string;
  spectrogramUri: string;
  location: Location;
  timestamp: Scalars["DateTime"]["output"];
  annotations: Annotation[];
  reviewed: boolean;
  found: string;
  comments: string | null | undefined;
  confidence: number;
  moderator: string;
  moderated: string | number; // string for Orcahello, number for Cascadia
  tags: string;
}
export interface AIData extends AIDetection {
  type: string;
  hydrophone: string;
  newCategory: string;
  timestampString: string;
}

interface CascadiaSighting {
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
  type: string;
  hydrophones: string[];
  newCategory: string;
  timestampString: string;
}

export interface CombinedData extends HumanData, AIData, Sighting {}

export interface Dataset {
  human: HumanData[];
  ai: AIData[];
  sightings: Sighting[];
  combined: CombinedData[];
  feeds: Feed[];
  isSuccessOrcahello: boolean;
  setNowPlaying?: Dispatch<SetStateAction<Candidate>>;
}

interface Location {
  name: string;
}
interface Annotation {
  id: number;
  startTime: number;
  endTime: number;
  confidence: number;
}

export interface Candidate {
  id: string;
  array: CombinedData[];
  startTimestamp: string;
  endTimestamp: string;
  whale: number;
  vessel: number;
  other: number;
  "whale (AI)": number;
  sightings: number;
  hydrophone: string;
  feedId: string | undefined;
  clipCount: string;
  duration: string;
  descriptions: string;
}
