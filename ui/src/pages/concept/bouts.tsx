import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import BoutsPage from "@/pages/bouts";

const HalfMapLearnPage: NextPageWithLayout = () => {
  return <BoutsPage />;
};

HalfMapLearnPage.getLayout = getModeratorLayout;

export default HalfMapLearnPage;
