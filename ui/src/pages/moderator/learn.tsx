import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import LearnPage from "@/pages/learn";

const ModeratorLearnPage: NextPageWithLayout = () => {
  return <LearnPage />;
};

ModeratorLearnPage.getLayout = getLeftNavLayout;

export default ModeratorLearnPage;
