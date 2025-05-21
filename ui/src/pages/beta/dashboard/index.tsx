import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import { LayoutContext } from "@/context/LayoutContext";
import type { NextPageWithLayout } from "@/pages/_app";
import Candidates from "@/pages/moderator/candidates";

const BetaPage: NextPageWithLayout = () => {
  return (
    <LayoutContext.Provider value="leftNav">
      <Candidates />
    </LayoutContext.Provider>
  );
};

BetaPage.getLayout = getLeftNavLayout;

export default BetaPage;
