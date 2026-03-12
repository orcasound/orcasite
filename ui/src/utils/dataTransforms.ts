import { Feed } from "@/graphql/generated";
import {
  AIDetection,
  AIDetectionRaw,
  AudioDetection,
  CascadiaSighting,
  DetectionsResult,
  Sighting,
} from "@/types/DataTypes";

import {
  lookupFeedId,
  lookupFeedName,
  lookupFeedSlug,
  standardizeFeedName,
} from "./dataHelpers";

export function transformAudioDetections(
  detections: DetectionsResult[],
  feeds: Feed[],
): AudioDetection[] {
  if (!feeds.length || !detections.length) return [];

  return detections.map((el) => ({
    ...el,
    type: "audio",
    standardizedFeedName: lookupFeedName(el.feedId!, feeds),
    feedSlug: lookupFeedSlug(el.feedId!, feeds),
    comments: el.description,
  }));
}

export function transformSightings(
  sightings: CascadiaSighting[] | undefined,
  feeds: Feed[],
  radius?: number,
): Sighting[] {
  if (!sightings || !sightings.length || !feeds.length) return [];
  // standardize data
  if (radius === undefined) radius = 3; // default radius in miles for assigning sightings to hydrophones
  const addLat = radius / 69;
  const addLong = (lat: number) =>
    radius / (69 * Math.cos((lat * Math.PI) / 180));

  const feedBoundingBoxes = feeds.map((feed) => ({
    name: feed.name,
    lat: feed.latLng.lat,
    lng: feed.latLng.lng,
    minLat: feed.latLng.lat - addLat,
    maxLat: feed.latLng.lat + addLat,
    minLng: feed.latLng.lng - addLong(feed.latLng.lat),
    maxLng: feed.latLng.lng + addLong(feed.latLng.lat),
  }));

  const assignSightingToHydrophone = (sighting: CascadiaSighting) => {
    let hydrophone = "out of range";
    feedBoundingBoxes.forEach((feed) => {
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

  const transformedSightings: Sighting[] = sightings.map((el): Sighting => {
    const feedName = assignSightingToHydrophone(el);
    const feedId = lookupFeedId(feedName, feeds);
    const feedSlug = lookupFeedSlug(feedId, feeds);

    return {
      ...el,
      type: "sightings",
      standardizedFeedName: feedName,
      feedSlug: feedSlug,
      feedId: feedId,
      timestamp: new Date(el.created.replace(" ", "T") + "Z"),
    };
  });

  return transformedSightings;
}

export const transformAIDetection = (
  raw: AIDetectionRaw,
  feeds: Feed[],
): AIDetection => {
  const standardizedFeedName = standardizeFeedName(
    raw.location?.name ?? "unknown",
  );
  const feedId = lookupFeedId(standardizedFeedName, feeds);

  return {
    ...raw,
    id: raw.id ?? crypto.randomUUID(),
    type: "ai",
    standardizedFeedName,
    feedId,
    feedSlug: lookupFeedSlug(feedId, feeds),
    comments: raw.comments,
    timestamp: new Date(raw.timestamp),
    annotations: raw.annotations ?? [],
    found:
      raw.found?.toLowerCase() === "yes"
        ? "yes"
        : raw.found?.toLowerCase() === "no"
          ? "no"
          : raw.found?.toLowerCase() === "don't know"
            ? "don't know"
            : null,
    reviewState: !raw.reviewed
      ? "unreviewed"
      : raw.found?.toLowerCase() === "yes"
        ? "confirmed"
        : raw.found?.toLowerCase() === "no"
          ? "falsepositive"
          : "unknown",
    tags: raw.tags
      ? raw.tags
          .split(";")
          .map((t) => t.trim())
          .filter(Boolean)
      : [],
  };
};
