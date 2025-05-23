import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import BoutsPage from "@/pages/bouts";

const ModeratorLearnPage: NextPageWithLayout = () => {
  return <BoutsPage />;
};

ModeratorLearnPage.getLayout = getLeftNavLayout;

export default ModeratorLearnPage;
