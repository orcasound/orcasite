import type { NextPageWithLayout } from "@/pages/_app";
import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";

import ModeratorCandidatesPage from "./candidates";

const ModeratorIndexPage: NextPageWithLayout = () => {
  return <ModeratorCandidatesPage />;
};

ModeratorCandidatesPage.getLayout = getLeftNavLayout;

export default ModeratorIndexPage;
