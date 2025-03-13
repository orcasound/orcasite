import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import DetectionsPage from "@/pages/reports";

const ModeratorMapPage: NextPageWithLayout = () => {
  return <DetectionsPage />;
};

ModeratorMapPage.getLayout = getModeratorLayout;

export default ModeratorMapPage;
