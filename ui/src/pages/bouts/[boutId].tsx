import Head from "next/head";
import { useParams, useSearchParams } from "next/navigation";

import BoutPage from "@/components/Bouts/BoutPage";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import { useBoutQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const BoutShowPage: NextPageWithLayout = () => {
  const params = useParams<{ boutId?: string }>();
  const boutId = params?.boutId;

  const searchParams = useSearchParams();
  const time = searchParams.get("time");
  let targetTime: Date | undefined;
  if (typeof time === "string" && !isNaN(new Date(time).getTime())) {
    targetTime = new Date(time);
  }

  const boutQueryResult = useBoutQuery(
    { id: boutId || "" },
    { enabled: !!boutId },
  );

  const bout = boutQueryResult.data?.bout;

  if (!boutId || boutQueryResult.isLoading) return <LoadingSpinner mt={5} />;
  if (!bout) return <p>Bout not found</p>;

  const feed = bout.feed;
  if (!feed) return <p>Feed not found</p>;

  return (
    <div>
      <Head>
        <title>Bout | Orcasound</title>
      </Head>

      <main>
        <BoutPage
          bout={bout}
          feed={feed}
          isNew={false}
          targetTime={targetTime}
        />
      </main>
    </div>
  );
};

BoutShowPage.getLayout = getSimpleLayout;

export default BoutShowPage;
