import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout";
import type { NextPageWithLayout } from "@/pages/_app";
import BoutsPage from "@/pages/bouts";

const NewFeedsPage: NextPageWithLayout = () => {
  return (
    <>
      <BoutsPage />
    </>
  );
};

NewFeedsPage.getLayout = getHalfMapLayout;

export default NewFeedsPage;
