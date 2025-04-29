import CandidatesList from "@/components/CandidateList/CandidatesList";
import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import { LayoutContext } from "@/context/LayoutContext";
import type { NextPageWithLayout } from "@/pages/_app";

const ModeratorCandidatesPage: NextPageWithLayout = () => {
  return (
    <LayoutContext.Provider value="leftNav">
      <CandidatesList />
    </LayoutContext.Provider>
  );
};

ModeratorCandidatesPage.getLayout = getLeftNavLayout;

export default ModeratorCandidatesPage;
