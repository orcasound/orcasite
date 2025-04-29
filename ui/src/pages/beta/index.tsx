import CandidatesList from "@/components/CandidateList/CandidatesList";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout";
import { LayoutContext } from "@/context/LayoutContext";
import type { NextPageWithLayout } from "@/pages/_app";

const BetaPage: NextPageWithLayout = () => {
  return (
    <LayoutContext.Provider value="halfMap">
      <CandidatesList />
    </LayoutContext.Provider>
  );
};

BetaPage.getLayout = getHalfMapLayout;

export default BetaPage;
