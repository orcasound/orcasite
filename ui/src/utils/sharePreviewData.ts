// Fetches the records that link previews describe. Server-side only: called
// from getStaticProps and from the /api/og image route.
//
// These fetch as an anonymous visitor, which is what a site fetching a preview
// is. A report that moderators have hidden gets the site's preview instead of
// its own. That is checked here: the API returns hidden candidates to anyone,
// because the policy that lets anyone read them comes before the one that
// would limit hidden ones to moderators.

import { isMissingRecordError } from "@/graphql/client";
import {
  useBoutQuery,
  useCandidateQuery,
  useFeedQuery,
} from "@/graphql/generated";

import {
  boutPreview,
  candidatePreview,
  feedPreview,
  type SharePreview,
} from "./sharePreview";

export async function loadFeedPreview(
  slug: string,
): Promise<SharePreview | null> {
  const { feed } = await useFeedQuery.fetcher({ slug })();
  return feed ? feedPreview(feed) : null;
}

// The page needs only the text, but the card also shows the feed's photo, which
// the candidate query doesn't include; `withFeedImage` fetches it as well.
export async function loadCandidatePreview(
  id: string,
  { withFeedImage = false } = {},
): Promise<SharePreview | null> {
  const { candidate } = await useCandidateQuery.fetcher({ id })();
  // Null counts as hidden too, as in the candidate's policies.
  if (!candidate || candidate.visible !== true) return null;

  let feedImageUrl: string | null | undefined;
  if (withFeedImage) {
    const { feed } = await useFeedQuery
      .fetcher({ slug: candidate.feed.slug })()
      .catch(() => ({ feed: null }));
    feedImageUrl = feed?.imageUrl;
  }

  return candidatePreview(candidate, feedImageUrl);
}

export async function loadBoutPreview(
  id: string,
): Promise<SharePreview | null> {
  const { bout } = await useBoutQuery.fetcher({ id })();
  return bout?.feed ? boutPreview({ ...bout, feed: bout.feed }) : null;
}

// For pages that render a preview when the record can be fetched and the
// site's preview otherwise. A transient API failure costs only a plainer
// preview for the next minute, since the page itself loads its data in the
// browser. A missing record is expected, so only other failures are logged.
// `field` is the query's field name, as isMissingRecordError needs it.
export async function previewOrNull(
  load: () => Promise<SharePreview | null>,
  field: string,
  id: string,
): Promise<SharePreview | null> {
  try {
    return await load();
  } catch (error) {
    if (!isMissingRecordError(error, field)) {
      console.warn(`No link preview for ${field} ${id}:`, error);
    }
    return null;
  }
}
