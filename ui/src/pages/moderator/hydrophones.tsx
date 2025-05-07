import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import FeedsPage from "@/pages/listen";

const ModeratorFeedsPage: NextPageWithLayout = () => {
  return <FeedsPage />;
};

ModeratorFeedsPage.getLayout = getLeftNavLayout;

export default ModeratorFeedsPage;
