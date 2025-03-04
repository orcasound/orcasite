import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import { useData } from "@/context/DataContext";
import { useFeedsQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const JSONPage: NextPageWithLayout = () => {
  const { combined } = useData();
  // get hydrophone feed list
  const feedsQueryResult = useFeedsQuery();
  const feeds = feedsQueryResult.data?.feeds ?? [];

  return (
    <>
      <pre>{JSON.stringify(combined, null, 2)}</pre>
      <pre>{JSON.stringify(feeds, null, 2)}</pre>
    </>
  );
};

JSONPage.getLayout = getModeratorLayout;

export default JSONPage;
