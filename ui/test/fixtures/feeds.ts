import type { FeedPartsFragment } from "@/graphql/generated";

// Defaults are valid and boring; a test overrides only the fields its assertion is
// about. The id is explicit so a test's rows don't depend on what ran before it
export function buildFeed(
  id: string,
  overrides: Partial<FeedPartsFragment> = {},
): FeedPartsFragment {
  return {
    id,
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
