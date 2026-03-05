import Head from "next/head";
import { ReactElement } from "react";

import SelectionPanel from "@/components/Shipnoise/SelectionPanel";
import { getShipnoiseLayout } from "@/components/Shipnoise/ShipnoiseLayout";
import { type NextPageWithLayout } from "@/pages/_app";

const ShipnoisePage: NextPageWithLayout = () => {
  return (
    <>
      <Head>
        <title>Shipnoise</title>
        <meta
          name="description"
          content="Explore underwater vessel noise recordings from Orcasound hydrophone network"
        />
      </Head>
      {/* SelectionPanel manages its own layout and padding */}
      <SelectionPanel />
    </>
  );
};

ShipnoisePage.getLayout = function getLayout(page: ReactElement) {
  return getShipnoiseLayout(page);
};

export default ShipnoisePage;
