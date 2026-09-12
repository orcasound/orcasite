import type { FeedPartsFragment } from "@/graphql/generated";

import { uniqueId } from "./sequence";

// Defaults are valid and boring; a test overrides only the fields its assertion is about
export function buildFeed(
  overrides: Partial<FeedPartsFragment> = {},
): FeedPartsFragment {
  return {
    id: uniqueId("feed"),
    name: "Orcasound Lab",
    slug: "orcasound-lab",
    nodeName: "rpi_orcasound_lab",
    latLng: { lat: 48.5583, lng: -123.1735 },
    introHtml: "",
    thumbUrl: "",
    imageUrl: "",
    mapUrl: "",
    bucket: "audio-orcasound-net",
    ...overrides,
  };
}
