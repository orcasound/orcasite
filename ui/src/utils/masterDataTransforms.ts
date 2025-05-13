import { Detection, Feed } from "@/graphql/generated";
import {
  AIData,
  AIDetection,
  CascadiaSighting,
  HumanData,
  Sighting,
} from "@/types/DataTypes";
import {
  lookupFeedId,
  lookupFeedName,
  standardizeFeedName,
} from "@/utils/masterDataHelpers";

export function transformHuman(
  humanDetections: Detection[],
  feeds: Feed[],
): HumanData[] {
  if (!feeds.length) return [];
  return humanDetections.map((el) => ({
    ...el,
    type: "human",
    hydrophone: lookupFeedName(el.feedId!, feeds),
    comments: el.description,
    newCategory: el.category!,
    timestampString: el.timestamp.toString(),
  }));
}

export function transformAI(
  aiDetections: AIDetection[],
  feeds: Feed[],
): AIData[] {
  return (
    aiDetections?.map((el) => ({
      ...el,
      type: "ai",
      hydrophone: standardizeFeedName(el.location.name),
      feedId: lookupFeedId(standardizeFeedName(el.location.name), feeds ?? []),
      newCategory: "WHALE (AI)",
      timestampString: el.timestamp.toString(),
    })) ?? []
  );
}

export function transformSightings(
  sightings: CascadiaSighting[],
  feeds: Feed[],
): Sighting[] {
  // standardize data
  const radius = 3;
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
    newCategory: "SIGHTINGS",
    hydrophone: assignSightingHydrophone(el),
    feedId: lookupFeedId(assignSightingHydrophone(el), feeds ?? []),
    timestampString: el.created.replace(" ", "T") + "Z",
    timestamp: new Date(el.created.replace(" ", "T") + "Z"),
  }));
}
