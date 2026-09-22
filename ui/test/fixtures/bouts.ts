import type { BoutPartsFragment, FeedPartsFragment } from "@/graphql/generated";

import { buildFeed } from "./feeds";

export type BoutFixture = BoutPartsFragment & { feed: FeedPartsFragment };

const FIVE_MINUTES = 5 * 60_000;

// Defaults are valid and boring; a test overrides only the fields its assertion is
// about. The id is explicit so a test's rows don't depend on what ran before it
export function buildBout(
  id: string,
  overrides: Partial<BoutFixture> = {},
): BoutFixture {
  const startTime = overrides.startTime ?? new Date("2026-01-01T12:00:00Z");

  return {
    id,
    name: "Bout",
    category: "BIOPHONY",
    startTime,
    endTime: new Date(startTime.getTime() + FIVE_MINUTES),
    duration: FIVE_MINUTES / 1000,
    feed: buildFeed(`${id}-feed`),
    ...overrides,
  };
}
