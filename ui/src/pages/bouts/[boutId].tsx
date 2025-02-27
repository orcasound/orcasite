import Head from "next/head";
import { useParams } from "next/navigation";

import BoutPage from "@/components/Bouts/BoutPage";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import LoadingSpinner from "@/components/LoadingSpinner";
import { useBoutQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const BoutShowPage: NextPageWithLayout = () => {
  const params = useParams<{ boutId?: string }>();
  const boutId = params?.boutId;

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
        <BoutPage bout={bout} feed={feed} isNew={false} />
      </main>
    </div>
  );
};

BoutShowPage.getLayout = getSimpleLayout;

export default BoutShowPage;
