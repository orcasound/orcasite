import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import DetectionsPage from "@/pages/reports";

const ModeratorReportsPage: NextPageWithLayout = () => {
  return <DetectionsPage />;
};

ModeratorReportsPage.getLayout = getModeratorLayout;

export default ModeratorReportsPage;
