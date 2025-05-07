import { getLeftNavLayout } from "@/components/layouts/LeftNavLayout";
import { useData } from "@/context/DataContext";
import type { NextPageWithLayout } from "@/pages/_app";

const JSONPage: NextPageWithLayout = () => {
  const { feeds, filteredData } = useData();

  return (
    <>
      <pre>{JSON.stringify(filteredData, null, 2)}</pre>
      <pre>{JSON.stringify(feeds, null, 2)}</pre>
    </>
  );
};

JSONPage.getLayout = getLeftNavLayout;

export default JSONPage;
