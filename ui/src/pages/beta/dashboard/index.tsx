import { CandidatesStack } from "@/components/CandidateList/CandidatesStack";
import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import type { NextPageWithLayout } from "@/pages/_app";

const BetaPage: NextPageWithLayout = () => {
  return <CandidatesStack />;
};

BetaPage.getLayout = getLeftNavLayout;

export default BetaPage;
