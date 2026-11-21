// What a page shows when its link is shared on Facebook, Bluesky, Slack and the
// like: the text of its Open Graph tags, and the content of the card image that
// /api/og draws for it. Plain functions, so the page and the image route agree
// on the wording and tests can check it without a server.
//
// Sites keep a preview long after they first fetch it. Facebook reuses it for
// weeks, and Bluesky copies it into the post. So a feed's preview says only
// what stays true about the feed. A report or bout describes something that
// already happened, so its preview can say what was heard and when.

import type { AudioCategory, DetectionCategory } from "@/graphql/generated";

export type SharePreview = {
  title: string;
  description: string;
  // Path of the page being shared, and of its card image, relative to the site.
  path: string;
  imagePath: string;
  card: {
    // A short line above the heading, naming what kind of page this is.
    eyebrow: string;
    heading: string;
    detail: string | null;
    // The feed's photo, drawn beside the text when there is one.
    thumbUrl: string | null;
  };
  // Null rather than undefined throughout: pages pass this from
  // getStaticProps, which can't send undefined.
};

export const CARD_WIDTH = 1200;
export const CARD_HEIGHT = 630;

type FeedInput = {
  slug: string;
  name: string;
  introHtml?: string | null;
  imageUrl?: string | null;
};

// The generated types call times Dates, but JSON carries them as strings, so
// either is accepted.
type Time = string | Date;

type CandidateInput = {
  id: string;
  minTime: Time;
  maxTime: Time;
  category?: DetectionCategory | null;
  detectionCount?: number | null;
  feed: { name: string };
};

type BoutInput = {
  id: string;
  name?: string | null;
  category: AudioCategory;
  startTime: Time;
  endTime?: Time | null;
  feed: { name: string; imageUrl?: string | null };
};

const DEFAULT_DESCRIPTION =
  "Listen live to underwater microphones in the Salish Sea, and help find the sounds of orcas.";

// The full URL of the site serving this request, for tags that need one.
// og:image and og:url must be absolute, and the build is promoted from one
// Heroku app to the next, so the host is read on the server when the page is
// rendered, never fixed at build time. server/load_env.sh and each app's config
// supply HOST_URL; it has no scheme.
export function siteOrigin(): string {
  const host = process.env.HOST_URL;
  if (host) return host.includes("://") ? host : `https://${host}`;
  return `http://localhost:${process.env.UI_PORT || 3000}`;
}

export function sitePreview(): SharePreview {
  return {
    title: "Orcasound",
    description: DEFAULT_DESCRIPTION,
    path: "/",
    imagePath: "/api/og/site",
    card: {
      eyebrow: "Live hydrophones",
      heading: "Listen for orcas in the Salish Sea",
      detail: null,
      thumbUrl: null,
    },
  };
}

export function feedPreview(feed: FeedInput): SharePreview {
  return {
    title: `Listen live: ${feed.name}`,
    description:
      firstSentence(feed.introHtml) ??
      `Listen live to the ${feed.name} hydrophone in the Salish Sea.`,
    path: `/listen/${encodeURIComponent(feed.slug)}`,
    imagePath: `/api/og/feed/${encodeURIComponent(feed.slug)}`,
    card: {
      eyebrow: "Listen live",
      heading: feed.name,
      detail: "Live sound from an underwater microphone",
      thumbUrl: feed.imageUrl ?? null,
    },
  };
}

const DETECTION_LABELS: Record<DetectionCategory, string> = {
  WHALE: "Whale sounds",
  VESSEL: "Vessel noise",
  OTHER: "Other sounds",
};

export function candidatePreview(
  candidate: CandidateInput,
  feedImageUrl?: string | null,
): SharePreview {
  const label = candidate.category
    ? DETECTION_LABELS[candidate.category]
    : "Sounds";
  const count = candidate.detectionCount ?? 1;
  const reports =
    count === 1 ? "1 listener report" : `${count} listener reports`;
  const when = formatTimeRange(candidate.minTime, candidate.maxTime);
  const title = `${label} reported at ${candidate.feed.name}`;

  return {
    title,
    description: `${reports}, ${when}.`,
    path: `/reports/${encodeURIComponent(candidate.id)}`,
    imagePath: `/api/og/report/${encodeURIComponent(candidate.id)}`,
    card: {
      eyebrow: "Listener report",
      heading: title,
      detail: when,
      thumbUrl: feedImageUrl ?? null,
    },
  };
}

const AUDIO_LABELS: Record<AudioCategory, string> = {
  BIOPHONY: "Marine life",
  ANTHROPHONY: "Human-made sounds",
  GEOPHONY: "Wind, rain and waves",
};

export function boutPreview(bout: BoutInput): SharePreview {
  const label = AUDIO_LABELS[bout.category];
  const when = formatTimeRange(bout.startTime, bout.endTime ?? bout.startTime);
  const length = bout.endTime
    ? `, ${formatDuration(bout.startTime, bout.endTime)}`
    : "";
  const heading = bout.name?.trim() || `${label} at ${bout.feed.name}`;

  return {
    title: heading,
    description: `${label} heard at ${bout.feed.name}, ${when}${length}.`,
    path: `/bouts/${encodeURIComponent(bout.id)}`,
    imagePath: `/api/og/bout/${encodeURIComponent(bout.id)}`,
    card: {
      eyebrow: `${label} · ${bout.feed.name}`,
      heading,
      detail: when,
      thumbUrl: bout.feed.imageUrl ?? null,
    },
  };
}

// Every hydrophone is in the Pacific Northwest, so times are given there. The
// time zone is explicit because servers run on UTC.
const timeFormat = new Intl.DateTimeFormat("en-US", {
  timeZone: "America/Los_Angeles",
  month: "short",
  day: "numeric",
  year: "numeric",
  hour: "numeric",
  minute: "2-digit",
  timeZoneName: "short",
});

// "Nov 21, 2026, 3:40 PM PST", or "Nov 21, 2026, 3:40 – 3:52 PM PST" for a range
// within one day. Intl puts narrow and thin spaces around the time; plain spaces
// read the same and are in every font the card might be drawn with.
export function formatTimeRange(start: Time, end: Time): string {
  const startDate = new Date(start);
  const endDate = new Date(end);
  const text =
    endDate.getTime() - startDate.getTime() < 60_000
      ? timeFormat.format(startDate)
      : timeFormat.formatRange(startDate, endDate);
  return text.replace(/[  ]/g, " ");
}

// "1 minute", "29 minutes", "1 hour 9 minutes". Bouts are rarely under a
// minute, and seconds would be noise in a preview, so this rounds to minutes.
export function formatDuration(start: Time, end: Time): string {
  const minutes = Math.max(
    1,
    Math.round((new Date(end).getTime() - new Date(start).getTime()) / 60_000),
  );
  const plural = (n: number, unit: string) =>
    `${n} ${unit}${n === 1 ? "" : "s"}`;
  if (minutes < 60) return plural(minutes, "minute");
  const hours = Math.floor(minutes / 60);
  const rest = minutes % 60;
  return rest
    ? `${plural(hours, "hour")} ${plural(rest, "minute")}`
    : plural(hours, "hour");
}

const MAX_DESCRIPTION = 200;

// The first sentence of a feed's intro, as plain text. The intro is HTML that
// moderators write, so this strips tags and decodes the common entities.
export function firstSentence(html?: string | null): string | undefined {
  if (!html) return undefined;
  const text = html
    .replace(/<[^>]*>/g, " ")
    .replace(/&nbsp;/g, " ")
    .replace(/&quot;/g, '"')
    .replace(/&#39;|&apos;/g, "'")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&amp;/g, "&")
    .replace(/\s+/g, " ")
    .trim();
  if (!text) return undefined;

  const sentence = text.match(/^.*?[.!?](?=\s|$)/)?.[0] ?? text;
  if (sentence.length <= MAX_DESCRIPTION) return sentence;
  const cut = sentence.slice(0, MAX_DESCRIPTION - 1);
  return `${cut.slice(0, cut.lastIndexOf(" "))}…`;
}
