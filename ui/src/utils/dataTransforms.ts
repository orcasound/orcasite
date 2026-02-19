import { Feed } from "@/graphql/generated";
import {
  AudioDetection,
  CascadiaSighting,
  DetectionsResult,
  Sighting,
} from "@/types/DataTypes";

import {
  lookupFeedId,
  lookupFeedName,
  standardizeFeedName,
} from "./dataHelpers";

const toNewCategory = (
  detection: DetectionsResult,
): AudioDetection["newCategory"] => {
  if (detection.source === "MACHINE") return "WHALE (AI)";

  switch (detection.category) {
    case "WHALE":
      return "WHALE (HUMAN)";
    case "VESSEL":
    case "OTHER":
      return detection.category;
    default:
      return "uncategorized";
  }
};

export function transformAudioDetections(
  detections: DetectionsResult[],
  feeds: Feed[],
): AudioDetection[] {
  if (!feeds.length) return [];

  return detections.map((el) => ({
    ...el,
    type: "audio",
    hydrophone: lookupFeedName(el.feedId!, feeds),
    comments: el.description,
    newCategory: toNewCategory(el),
    timestampString: el.timestamp.toString(),
  }));
}

export function transformSightings(
  sightings: CascadiaSighting[],
  feeds: Feed[],
  radius?: number,
): Sighting[] {
  // standardize data
  if (radius === undefined) radius = 3; // default radius in miles for assigning sightings to hydrophones
  const addLat = radius / 69;
  const addLong = (lat: number) =>
    radius / (69 * Math.cos((lat * Math.PI) / 180));

  const feedCoordinates = feeds.map((feed) => ({
    name: feed.name,
    lat: feed.latLng.lat,
    lng: feed.latLng.lng,
    minLat: feed.latLng.lat - addLat,
    maxLat: feed.latLng.lat + addLat,
    minLng: feed.latLng.lng - addLong(feed.latLng.lat),
    maxLng: feed.latLng.lng + addLong(feed.latLng.lat),
  }));

  const assignSightingHydrophone = (sighting: CascadiaSighting) => {
    let hydrophone: string = "out of range";
    feedCoordinates.forEach((feed) => {
      const inLatRange =
        sighting.latitude >= feed.minLat && sighting.latitude <= feed.maxLat;
      const inLngRange =
        sighting.longitude >= feed.minLng && sighting.longitude <= feed.maxLng;
      if (inLatRange && inLngRange) {
        hydrophone = feed.name;
      }
    });
    hydrophone = standardizeFeedName(hydrophone);
    return hydrophone;
  };

  if (!Array.isArray(sightings)) return [];

  return sightings.map((el) => ({
    ...el,
    type: "sightings",
    newCategory: "SIGHTING",
    hydrophone: assignSightingHydrophone(el),
    feedId: lookupFeedId(assignSightingHydrophone(el), feeds ?? []),
    timestampString: el.created.replace(" ", "T") + "Z",
    timestamp: new Date(el.created.replace(" ", "T") + "Z"),
  }));
}
