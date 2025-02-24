import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import type { NextPageWithLayout } from "@/pages/_app";

import Candidates from "./candidates";

const NewFeedsPage: NextPageWithLayout = () => {
  return (
    <>
      <Candidates />
    </>
  );
};

NewFeedsPage.getLayout = getModeratorLayout;

export default NewFeedsPage;
