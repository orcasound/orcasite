import { Dispatch, SetStateAction } from "react";

import { Detection, Scalars } from "@/graphql/generated";
import { Candidate } from "@/pages/moderator/candidates";

export interface HumanData extends Omit<Detection, "candidate"> {
  type: string;
  hydrophone: string;
  comments: string | null | undefined;
  newCategory: string;
  timestampString: string;
}

export interface AIDetection {
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
  moderated: string;
  tags: string;
}
export interface AIData extends AIDetection {
  type: string;
  hydrophone: string;
  newCategory: string;
  timestampString: string;
}
export interface CombinedData extends HumanData, AIData {}

export interface Dataset {
  human: HumanData[];
  ai: AIData[];
  combined: CombinedData[];
  // feeds: Feed[];
  isSuccess: boolean;
  setNowPlaying?: Dispatch<SetStateAction<Candidate>>;
}

export interface Location {
  name: string;
}
export interface Annotation {
  id: number;
  startTime: number;
  endTime: number;
  confidence: number;
}
