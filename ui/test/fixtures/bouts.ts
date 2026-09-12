import type { BoutPartsFragment, FeedPartsFragment } from "@/graphql/generated";

import { buildFeed } from "./feeds";
import { uniqueId } from "./sequence";

export type BoutFixture = BoutPartsFragment & { feed: FeedPartsFragment };

const FIVE_MINUTES = 5 * 60_000;
const HOUR = 60 * 60_000;

// Defaults are valid and boring; a test overrides only the fields its assertion is about
export function buildBout(overrides: Partial<BoutFixture> = {}): BoutFixture {
  const startTime = overrides.startTime ?? new Date("2026-01-01T12:00:00Z");

  return {
    id: uniqueId("bout"),
    name: "Bout",
    category: "BIOPHONY",
    startTime,
    endTime: new Date(startTime.getTime() + FIVE_MINUTES),
    duration: FIVE_MINUTES / 1000,
    feed: buildFeed(),
    ...overrides,
  };
}

/** `count` ended bouts an hour apart, newest first: "Bout 1" is the most recent. */
export function buildEndedBouts(count: number): BoutFixture[] {
  const newest = new Date("2026-01-01T12:00:00Z").getTime();

  return Array.from({ length: count }, (_, index) =>
    buildBout({
      name: `Bout ${index + 1}`,
      startTime: new Date(newest - index * HOUR),
    }),
  );
}
