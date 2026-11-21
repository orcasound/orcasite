// Draws the 1200×630 card image that a link preview shows (og:image):
//
//   /api/og/site
//   /api/og/feed/<slug>
//   /api/og/report/<candidate id>
//   /api/og/bout/<bout id>
//
// A card shows the Orcasound wordmark, what the page is, and the feed's photo
// when there is one. The photos are small squares (most are 200×200), too
// small to stand alone as a wide preview image, which is why this draws a card
// around them. API routes run on each request in the serving app, so the card
// always describes that app's data.
//
// The text comes from the same functions the pages use for their tags
// (utils/sharePreview.ts), so the card and the preview's text agree.

// ImageResponse draws plain <img> elements; next/image has no meaning here.
/* eslint-disable @next/next/no-img-element */

import { readFileSync } from "node:fs";
import path from "node:path";

import type { NextApiRequest, NextApiResponse } from "next";
import { ImageResponse } from "next/og";

import {
  CARD_HEIGHT,
  CARD_WIDTH,
  type SharePreview,
  sitePreview,
} from "@/utils/sharePreview";
import {
  loadBoutPreview,
  loadCandidatePreview,
  loadFeedPreview,
  previewOrNull,
} from "@/utils/sharePreviewData";

const COLORS = {
  background: "#080d26",
  backgroundEnd: "#002f49",
  eyebrow: "#a4d3d1",
  heading: "#ffffff",
  detail: "#c9d4e0",
};

// Next runs from ui/ (see server/config/runtime.exs), so public/ is here.
const wordmark = `data:image/svg+xml;base64,${readFileSync(
  path.join(process.cwd(), "public/wordmark/wordmark-white.svg"),
).toString("base64")}`;

const LOADERS: Record<
  string,
  [string, (id: string) => Promise<SharePreview | null>]
> = {
  feed: ["feed", loadFeedPreview],
  report: [
    "candidate",
    (id) => loadCandidatePreview(id, { withFeedImage: true }),
  ],
  bout: ["bout", loadBoutPreview],
};

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  const [kind, id, ...rest] = ([] as string[]).concat(req.query.params ?? []);

  let preview: SharePreview | null = null;
  if (kind === "site" && !id) {
    preview = sitePreview();
  } else if (kind && id && !rest.length && Object.hasOwn(LOADERS, kind)) {
    const [field, load] = LOADERS[kind];
    preview = await previewOrNull(() => load(id), field, id);
  } else {
    res.status(404).end();
    return;
  }

  // A record that couldn't be loaded (missing, hidden, or the API failing)
  // gets the site's card, kept briefly so a passing failure doesn't stick.
  const found = preview !== null;
  const card = (preview ?? sitePreview()).card;
  const thumb = card.thumbUrl ? await imageDataUrl(card.thumbUrl) : undefined;

  const image = new ImageResponse(<Card card={card} thumb={thumb} />, {
    width: CARD_WIDTH,
    height: CARD_HEIGHT,
  });
  const body = Buffer.from(await image.arrayBuffer());

  res.setHeader("Content-Type", "image/png");
  res.setHeader(
    "Cache-Control",
    found ? "public, max-age=86400" : "public, max-age=300",
  );
  res.status(200).send(body);
}

function Card({ card, thumb }: { card: SharePreview["card"]; thumb?: string }) {
  return (
    <div
      style={{
        width: "100%",
        height: "100%",
        display: "flex",
        flexDirection: "column",
        justifyContent: "space-between",
        padding: 72,
        backgroundImage: `linear-gradient(135deg, ${COLORS.background} 40%, ${COLORS.backgroundEnd} 100%)`,
      }}
    >
      {/* The wordmark's viewBox is 980×144. */}
      <img src={wordmark} width={327} height={48} alt="" />

      <div style={{ display: "flex", alignItems: "flex-end", gap: 56 }}>
        <div style={{ display: "flex", flexDirection: "column", flex: 1 }}>
          <div
            style={{
              color: COLORS.eyebrow,
              fontSize: 30,
              letterSpacing: 2,
              textTransform: "uppercase",
              marginBottom: 20,
            }}
          >
            {card.eyebrow}
          </div>
          <div
            style={{
              color: COLORS.heading,
              fontSize: headingSize(card.heading),
              lineHeight: 1.1,
            }}
          >
            {truncate(card.heading, 90)}
          </div>
          {card.detail && (
            <div style={{ color: COLORS.detail, fontSize: 34, marginTop: 28 }}>
              {card.detail}
            </div>
          )}
        </div>
        {thumb && (
          <img
            src={thumb}
            width={300}
            height={300}
            alt=""
            style={{ borderRadius: 24, objectFit: "cover" }}
          />
        )}
      </div>
    </div>
  );
}

// Smaller type for longer headings, so a bout's name fits in three lines.
function headingSize(text: string): number {
  if (text.length <= 30) return 76;
  if (text.length <= 60) return 64;
  return 52;
}

function truncate(text: string, max: number): string {
  if (text.length <= max) return text;
  const cut = text.slice(0, max - 1);
  return `${cut.slice(0, cut.lastIndexOf(" "))}…`;
}

const MAX_THUMB_BYTES = 5_000_000;

// The feed's photo as a data URL, or nothing if it can't be fetched quickly.
// Fetched here rather than left to the renderer, which would fail the whole
// card if the photo were missing or slow.
async function imageDataUrl(url: string): Promise<string | undefined> {
  try {
    const response = await fetch(url, { signal: AbortSignal.timeout(5000) });
    const type = response.headers.get("content-type") ?? "";
    if (!response.ok || !/^image\/(png|jpeg)$/.test(type)) return undefined;
    const bytes = await readAtMost(response, MAX_THUMB_BYTES);
    return bytes && `data:${type};base64,${bytes.toString("base64")}`;
  } catch {
    return undefined;
  }
}

// The response's body, or nothing if it is longer than `limit`, in which case
// the download stops there rather than holding all of it in memory.
async function readAtMost(
  response: Response,
  limit: number,
): Promise<Buffer | undefined> {
  if (!response.body) return undefined;
  const reader = response.body.getReader();
  const chunks: Uint8Array[] = [];
  let length = 0;
  for (;;) {
    const { done, value } = await reader.read();
    if (done) return Buffer.concat(chunks);
    length += value.length;
    if (length > limit) {
      await reader.cancel();
      return undefined;
    }
    chunks.push(value);
  }
}
