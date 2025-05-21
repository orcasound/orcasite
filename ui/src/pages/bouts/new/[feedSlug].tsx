import Head from "next/head";
import { useParams, useSearchParams } from "next/navigation";

import BoutPage from "@/components/Bouts/BoutPage";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import { AudioCategory, useFeedQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const NewBoutPage: NextPageWithLayout = () => {
  const params = useParams<{ feedSlug?: string }>();

  const searchParams = useSearchParams();

  const feedSlug = params?.feedSlug;
  const time = searchParams.get("time");
  let targetTime: Date | undefined;
  if (typeof time === "string" && !isNaN(new Date(time).getTime())) {
    targetTime = new Date(time);
  }
  const audioCategories = Object.values(AudioCategory);
  const categoryParam = searchParams.get("category");
  const targetAudioCategory = audioCategories.find(
    (category) => category === categoryParam?.toUpperCase(),
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
