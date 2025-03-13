import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import BoutsPage from "@/pages/bouts";

const ModeratorLearnPage: NextPageWithLayout = () => {
  return <BoutsPage />;
};

ModeratorLearnPage.getLayout = getModeratorLayout;

export default ModeratorLearnPage;
