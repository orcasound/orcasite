import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import LearnPage from "@/pages/learn";

const ModeratorLearnPage: NextPageWithLayout = () => {
  return <LearnPage />;
};

ModeratorLearnPage.getLayout = getModeratorLayout;

export default ModeratorLearnPage;
