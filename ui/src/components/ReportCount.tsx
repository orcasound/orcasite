import { Breadcrumbs, Button } from "@mui/material";

import { CombinedData } from "@/types/DataTypes";

const countCategories = (
  arr: { newCategory?: string | null }[],
  cat: string,
) => {
  if (!arr.length) {
    return 0;
  }
  return arr.filter(
    (d) => (d.newCategory ?? "").toLowerCase() === cat.toLowerCase(),
  ).length;
};

export default function ReportCount({
  detectionArray,
}: {
  detectionArray: CombinedData[];
}) {
  const categories = [
    "whale (human)",
    "whale (AI)",
    "vessel",
    "other",
    "sighting",
  ];

  const items = categories
    .map((category) => {
      const count = countCategories(detectionArray, category);

      let label = category;
      if (category === "sighting" && count !== 1) {
        label += "s in audible range";
      } else if (category === "sighting") {
        label += " in audible range";
      }

      return (
        <div key={category}>
          {count} {label}
        </div>
      );
    })
    .filter((c) => c); // filters out the null items

  items.unshift(<strong>Last 7 days</strong>);

  // Interleave with separators
  const interleaved = items.flatMap((item, index) =>
    index < items.length - 1
      ? [item, <span key={`dot-${index}`}> • </span>]
      : [item],
  );

  return (
    <>
      <div
        style={{
          lineHeight: 1.6,
          marginBottom: "18px",
          display: "flex",
          columnGap: "8px",
          flexWrap: "wrap",
        }}
      >
        {interleaved}
      </div>
      <Breadcrumbs>
        <Button href={`/reports`} variant="contained">
          View all reports
        </Button>
      </Breadcrumbs>
    </>
  );
}
