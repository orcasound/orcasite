import { describe, expect, it } from "vitest";

import type { Feed } from "@/graphql/generated";
import type { CascadiaSighting, DetectionsResult } from "@/types/DataTypes";

import { transformAudioDetections, transformSightings } from "./dataTransforms";

const makeFeed = (
  id: string,
  name: string,
  slug: string,
  lat: number,
  lng: number,
): Feed =>
  ({
    id,
    name,
    slug,
    latLng: { lat, lng },
  }) as unknown as Feed;

const makeDetection = (
  overrides: Partial<DetectionsResult> = {},
): DetectionsResult =>
  ({
    id: "d-1",
    feedId: "f-1",
    source: "HUMAN",
    category: "WHALE",
    description: "demo",
    playlistTimestamp: 123,
    playerOffset: 2,
    timestamp: new Date("2025-01-01T00:00:00Z"),
    ...overrides,
  }) as DetectionsResult;

const makeSighting = (
  overrides: Partial<CascadiaSighting> = {},
): CascadiaSighting => ({
  id: "s-1",
  type: "sighting",
  project_id: 1,
  trip_id: 1,
  name: "Killer Whale (Orca)",
  scientific_name: "Orcinus orca",
  number_sighted: 1,
  latitude: 48.1,
  longitude: -122.75,
  created: "2025-01-01 17:25:00",
  source: "whale_alert",
  comments: "test",
  icon: "dot-black",
  photo_url: "",
  usernm: "tester",
  count_check: 0,
  in_ocean: 1,
  is_test: 0,
  moderated: 1,
  trusted: 1,
  ...overrides,
});

describe("transformAudioDetections", () => {
  it("returns empty array when inputs are missing", () => {
    expect(transformAudioDetections([], [])).toEqual([]);
  });

  it("maps source/category to newCategory and enriches feed metadata", () => {
    const feeds = [makeFeed("f-1", "North SJC", "north-sjc", 48.1, -122.75)];
    const machine = makeDetection({
      id: "d-machine",
      source: "MACHINE",
      category: "WHALE",
    });
    const human = makeDetection({
      id: "d-human",
      source: "HUMAN",
      category: "WHALE",
    });

    const transformed = transformAudioDetections([machine, human], feeds);

    expect(transformed).toHaveLength(2);
    expect(transformed[0].newCategory).toBe("WHALE (AI)");
    expect(transformed[1].newCategory).toBe("WHALE (HUMAN)");
    expect(transformed[0].standardizedFeedName).toBe("North San Juan Channel");
    expect(transformed[0].feedSlug).toBe("north-sjc");
    expect(transformed[0].type).toBe("audio");
  });
});

describe("transformSightings", () => {
  it("assigns matching feed metadata for in-range sightings", () => {
    const feeds = [makeFeed("f-1", "North SJC", "north-sjc", 48.1, -122.75)];
    const sightings = [makeSighting()];

    const transformed = transformSightings(sightings, feeds);

    expect(transformed).toHaveLength(1);
    expect(transformed[0].type).toBe("sightings");
    expect(transformed[0].newCategory).toBe("SIGHTING");
    expect(transformed[0].standardizedFeedName).toBe("North San Juan Channel");
    expect(transformed[0].feedId).toBe("f-1");
    expect(transformed[0].feedSlug).toBe("north-sjc");
    expect(transformed[0].timestampString).toBe("2025-01-01T17:25:00Z");
  });

  it("marks out-of-range sightings with fallback feed identifiers", () => {
    const feeds = [makeFeed("f-1", "North SJC", "north-sjc", 48.1, -122.75)];
    const sightings = [
      makeSighting({
        latitude: 0,
        longitude: 0,
      }),
    ];

    const transformed = transformSightings(sightings, feeds);

    expect(transformed[0].standardizedFeedName).toBe("out of range");
    expect(transformed[0].feedId).toBe("feed id not found");
    expect(transformed[0].feedSlug).toBe("feed slug not found");
  });
});
