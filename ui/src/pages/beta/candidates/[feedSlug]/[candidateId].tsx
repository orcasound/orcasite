import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import type { NextPageWithLayout } from "@/pages/_app";

import CandidatePage from "../../[feedSlug]/[candidateId]";

const BetaPage: NextPageWithLayout = () => {
  return <CandidatePage />;
};

BetaPage.getLayout = getHalfMapLayout;

export default BetaPage;
