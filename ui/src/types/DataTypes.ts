import { Detection, Feed, Scalars } from "@/graphql/generated";

export interface HumanData extends Omit<Detection, "candidate"> {
  type: string;
  hydrophone: string;
  comments: string | null | undefined;
  newCategory: string;
  dateString: string;
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
  dateString: string;
}
export interface CombinedData extends HumanData, AIData {}

export interface Dataset {
  human: HumanData[];
  ai: AIData[];
  combined: CombinedData[];
  feeds: Feed[];
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
export interface Feed {}
