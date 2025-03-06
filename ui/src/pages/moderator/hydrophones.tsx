import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import FeedsPage from "@/pages/listen";

const ModeratorFeedsPage: NextPageWithLayout = () => {
  return <FeedsPage />;
};

ModeratorFeedsPage.getLayout = getModeratorLayout;

export default ModeratorFeedsPage;
