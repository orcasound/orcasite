import { useRouter } from "next/router";

import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import type { NextPageWithLayout } from "@/pages/_app";

const NewBoutPage: NextPageWithLayout = () => {
  const router = useRouter();
  const { feedId, category } = router.query;

  // Get feed, detections, recent spectrograms.

  return <p>New Bout</p>;
};

NewBoutPage.getLayout = getSimpleLayout;

export default NewBoutPage;
