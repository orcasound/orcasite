import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import DetectionsPage from "@/pages/reports";

const ModeratorMapPage: NextPageWithLayout = () => {
  return <DetectionsPage />;
};

ModeratorMapPage.getLayout = getHalfMapLayout;

export default ModeratorMapPage;
