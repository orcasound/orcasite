import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import { useData } from "@/context/DataContext";
import type { NextPageWithLayout } from "@/pages/_app";

const JSONPage: NextPageWithLayout = () => {
  const { human, ai, combined, feeds } = useData();
  // this hook uses a context provider to call data once and make it available to all children -- this may not be better than just using the query hooks, kind of does the same thing
  // some docs say that this is not needed because Next automatically detects whether a call has already been made, so you can just make as many as you want in children
  //  const feedsQueryResult = useFeedsQuery();
  //  const detectionQueryResult = useDetectionsQuery();
  //  const recentDetections = detectionQueryResult.data?.detections?.results ?? [];

  return (
    <>
      {/* the plan is to put these in tabs */}
      <pre>{JSON.stringify(feeds, null, 2)}</pre>
      <pre>{JSON.stringify(feeds, null, 2)}</pre>
      <pre>{JSON.stringify(human, null, 2)}</pre>
      <pre>{JSON.stringify(ai, null, 2)}</pre>
    </>
  );
};

JSONPage.getLayout = getModeratorLayout;

export default JSONPage;
