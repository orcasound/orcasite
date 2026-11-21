import Head from "next/head";
import { useParams } from "next/navigation";
import type { ReactNode } from "react";

import BoutPage from "@/components/Bouts/BoutPage";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import SharePreviewTags from "@/components/SharePreviewTags";
import { useBoutQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import {
  type SharePreview,
  siteOrigin,
  sitePreview,
} from "@/utils/sharePreview";
import { loadBoutPreview, previewOrNull } from "@/utils/sharePreviewData";

type Props = { preview: SharePreview | null; origin: string };

const BoutShowPage: NextPageWithLayout<Props> = ({ preview, origin }) => {
  const params = useParams<{ boutId?: string }>();
  const boutId = params?.boutId;

  const boutQueryResult = useBoutQuery(
    { id: boutId || "" },
    { enabled: !!boutId },
  );

  const bout = boutQueryResult.data?.bout;

  // In every branch: the server renders this page before the bout has loaded,
  // and the tags must be in that HTML.
  const withHead = (content: ReactNode) => (
    <>
      <Head>
        <title>{`${preview?.title ?? "Bout"} | Orcasound`}</title>
      </Head>
      <SharePreviewTags
        preview={preview ?? { ...sitePreview(), path: `/bouts/${boutId}` }}
        origin={origin}
      />
      {content}
    </>
  );

  if (!boutId || boutQueryResult.isLoading)
    return withHead(<LoadingSpinner sx={{ mt: 5 }} />);
  if (!bout) return withHead(<p>Bout not found</p>);

  const feed = bout.feed;
  if (!feed) return withHead(<p>Feed not found</p>);

  return withHead(
    <div>
      <main>
        <BoutPage bout={bout} feed={feed} isNew={false} />
      </main>
    </div>,
  );
};

BoutShowPage.getLayout = getSimpleLayout;

// Rendered on the server only for the link preview. The page still loads the
// bout in the browser as before; only the preview's few fields come from here.
export function getStaticPaths() {
  return { paths: [], fallback: "blocking" };
}

export async function getStaticProps({
  params,
}: {
  params: { boutId: string };
}) {
  const preview = await previewOrNull(
    () => loadBoutPreview(params.boutId),
    "bout",
    params.boutId,
  );

  return {
    props: { preview, origin: siteOrigin() } satisfies Props,
    // Moderators can rename a bout or change its times.
    revalidate: 60,
  };
}

export default BoutShowPage;
