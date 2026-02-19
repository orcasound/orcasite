import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import { useDetectionsQuery } from "@/graphql/generated";
import { useCombinedData } from "@/hooks/useCombinedData";

function JsonPage() {
  const detections = useDetectionsQuery().data?.detections?.results ?? [];
  const combinedData = useCombinedData().combined;

  return (
    <>
      {/* <pre>{JSON.stringify(detections, null, 2)}</pre> */}
      <pre>{JSON.stringify(combinedData, null, 2)}</pre>
    </>
  );
}

JsonPage.getLayout = getSimpleLayout;

export default JsonPage;
