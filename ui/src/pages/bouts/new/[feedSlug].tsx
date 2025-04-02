import _ from "lodash";
import Head from "next/head";
import { useParams, useSearchParams } from "next/navigation";

import BoutPage from "@/components/Bouts/BoutPage";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import { AudioCategory, useFeedQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const NewBoutPage: NextPageWithLayout = () => {
  const targetTime = new Date("2025-01-06 05:26:10.000Z");

  const params = useParams<{ feedSlug?: string }>();
  const feedSlug = params?.feedSlug;
  const searchParams = useSearchParams();
  const audioCategories = Object.values(AudioCategory);
  const categoryParam = searchParams.get("category");
 const targetAudioCategory = audioCategories.find(
    (category) => category === categoryParam?.toUpperCase()
  ) as AudioCategory | undefined;

  const feedQueryResult = useFeedQuery(
    { slug: feedSlug || "" },
    { enabled: !!feedSlug },
  );
  const feed = feedQueryResult.data?.feed;

  if (!feedSlug || feedQueryResult.isLoading) return <LoadingSpinner mt={5} />;
  if (!feed) return <p>Feed not found</p>;

  return (
    <div>
      <Head>
        <title>New Bout | Orcasound</title>
      </Head>

      <main>
        <BoutPage
          isNew={true}
          feed={feed}
          targetTime={targetTime}
          targetAudioCategory={targetAudioCategory}
        />
      </main>
    </div>
  );
};

NewBoutPage.getLayout = getSimpleLayout;

export default NewBoutPage;
