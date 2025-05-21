import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import DetectionsPage from "@/pages/reports";

const ModeratorReportsPage: NextPageWithLayout = () => {
  return <DetectionsPage />;
};

ModeratorReportsPage.getLayout = getLeftNavLayout;

export default ModeratorReportsPage;
