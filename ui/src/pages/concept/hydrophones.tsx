import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import FeedsPage from "@/pages/listen";

const HalfMapFeedsPage: NextPageWithLayout = () => {
  return <FeedsPage />;
};

HalfMapFeedsPage.getLayout = getHalfMapLayout;

export default HalfMapFeedsPage;
