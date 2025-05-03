import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout";
import type { NextPageWithLayout } from "@/pages/_app";

const BetaPage: NextPageWithLayout = () => {
  return null;
  // return (
  //   <LayoutContext.Provider value="halfMap">
  //     <CandidatesTabs />
  //   </LayoutContext.Provider>
  // );
};

BetaPage.getLayout = getHalfMapLayout;

export default BetaPage;
