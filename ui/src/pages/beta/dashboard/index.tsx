import { CandidatesStack } from "@/components/CandidateList/CandidatesStack";
import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import { LayoutContext } from "@/context/LayoutContext";
import type { NextPageWithLayout } from "@/pages/_app";

const BetaPage: NextPageWithLayout = () => {
  return (
    <LayoutContext.Provider value="leftNav">
      <CandidatesStack />
    </LayoutContext.Provider>
  );
};

BetaPage.getLayout = getLeftNavLayout;

export default BetaPage;
