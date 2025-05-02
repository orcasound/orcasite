import CandidatesTabs from "@/components/CandidateList/CandidatesTabs";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout";
import { LayoutContext } from "@/context/LayoutContext";
import type { NextPageWithLayout } from "@/pages/_app";

const BetaPage: NextPageWithLayout = () => {
  return (
    <LayoutContext.Provider value="halfMap">
      <CandidatesTabs />
    </LayoutContext.Provider>
  );
};

BetaPage.getLayout = getHalfMapLayout;

export default BetaPage;
