import _ from "lodash";
import Head from "next/head";
import { useParams, useSearchParams } from "next/navigation";
import { useMemo } from "react";

import BoutPage from "@/components/Bouts/BoutPage";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import { AudioCategory, useFeedQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const NewBoutPage: NextPageWithLayout = () => {
  const targetTime = new Date("2024-12-11 19:52:44.013Z");

  const params = useParams<{ feedSlug?: string }>();
  const feedSlug = params?.feedSlug;
  const searchParams = useSearchParams();
  const audioCategories: AudioCategory[] = useMemo(
    () => ["ANTHROPHONY", "BIOPHONY", "GEOPHONY"],
    [],
  );
  const categoryParam = searchParams.get("category");
  let targetAudioCategory;
  if (
    categoryParam &&
    audioCategories.includes(_.toUpper(categoryParam) as AudioCategory)
  ) {
    targetAudioCategory = categoryParam.toUpperCase() as AudioCategory;
  } else {
    targetAudioCategory = undefined;
  }

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
