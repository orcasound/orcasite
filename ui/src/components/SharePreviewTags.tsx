import Head from "next/head";

import {
  CARD_HEIGHT,
  CARD_WIDTH,
  type SharePreview,
} from "@/utils/sharePreview";

// The tags sites read to build a link preview: Open Graph for Facebook,
// Bluesky, Slack and most others, and twitter:card for X. Each has a `key` so
// a page's tags replace the site-wide ones in _app.tsx instead of repeating
// them.
//
// `origin` comes from getStaticProps or getServerSideProps (see siteOrigin):
// it must be the serving app's, and reading it there keeps the server's HTML
// and the browser's first render the same.
export default function SharePreviewTags({
  preview,
  origin,
}: {
  preview: SharePreview;
  origin: string;
}) {
  const imageUrl = `${origin}${preview.imagePath}`;

  return (
    <Head>
      <meta
        key="description"
        name="description"
        content={preview.description}
      />
      <meta key="og:title" property="og:title" content={preview.title} />
      <meta
        key="og:description"
        property="og:description"
        content={preview.description}
      />
      <meta
        key="og:url"
        property="og:url"
        content={`${origin}${preview.path}`}
      />
      <meta key="og:image" property="og:image" content={imageUrl} />
      <meta key="og:image:type" property="og:image:type" content="image/png" />
      <meta
        key="og:image:width"
        property="og:image:width"
        content={String(CARD_WIDTH)}
      />
      <meta
        key="og:image:height"
        property="og:image:height"
        content={String(CARD_HEIGHT)}
      />
      <meta
        key="og:image:alt"
        property="og:image:alt"
        content={preview.title}
      />
      <meta
        key="twitter:card"
        name="twitter:card"
        content="summary_large_image"
      />
    </Head>
  );
}
