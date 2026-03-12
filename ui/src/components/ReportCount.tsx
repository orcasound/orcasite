import { Box, Button } from "@mui/material";
import { useMemo } from "react";

import { useCombinedData } from "@/hooks/useCombinedData";
import { CombinedData } from "@/types/DataTypes";

const countHumanWhale = (arr: CombinedData[]) =>
  arr.filter(
    (d) => d.type === "audio" && d.source === "HUMAN" && d.category === "WHALE",
  ).length;

const countHumanVessel = (arr: CombinedData[]) =>
  arr.filter(
    (d) =>
      d.type === "audio" && d.source === "HUMAN" && d.category === "VESSEL",
  ).length;

const countHumanOther = (arr: CombinedData[]) =>
  arr.filter(
    (d) => d.type === "audio" && d.source === "HUMAN" && d.category === "OTHER",
  ).length;

const countSightings = (arr: CombinedData[]) =>
  arr.filter((d) => d.type === "sightings").length;

const countMachineCategory = (arr: CombinedData[], reviewState: string) =>
  arr.filter(
    (d) =>
      d.type === "ai" &&
      (d.reviewState ?? "").toLowerCase() === reviewState.toLowerCase(),
  ).length;

export default function ReportCount({ feedSlug }: { feedSlug?: string }) {
  const { combined } = useCombinedData();

  const combinedThisFeed = useMemo(() => {
    if (!feedSlug) {
      return combined;
    } else {
      return combined?.filter((f) => {
        return feedSlug === f.feedSlug;
      });
    }
  }, [combined, feedSlug]);

  //// Human count string
  const humanCategories = [
    { key: "whale", label: "whale", count: countHumanWhale(combinedThisFeed) },
    {
      key: "vessel",
      label: "vessel",
      count: countHumanVessel(combinedThisFeed),
    },
    { key: "other", label: "other", count: countHumanOther(combinedThisFeed) },
    {
      key: "sighting",
      label: "sighting",
      count: countSightings(combinedThisFeed),
    },
  ];

  const humanItems = humanCategories
    .map(({ key, label, count }) => {
      let displayLabel = label;

      if (key === "sighting" && count !== 1) {
        displayLabel += "s in audible range";
      } else if (key === "sighting") {
        displayLabel += " in audible range";
      }

      return (
        <div key={key}>
          {count} {displayLabel}
        </div>
      );
    })
    .filter((c) => c); // filters out the null items

  humanItems.unshift(<strong key="human-items">Human</strong>);

  const humanItemString = humanItems.flatMap((item, index) =>
    index < humanItems.length - 1
      ? [item, <span key={`dot-${index}`}> • </span>]
      : [item],
  );

  //// Machine count string
  const machineCategories = [
    "confirmed",
    "falsepositive",
    "unknown",
    "unreviewed",
  ];

  const machineItems = machineCategories
    .map((category) => {
      const count = countMachineCategory(combinedThisFeed, category);

      let label = category;
      if (category === "falsepositive" && count !== 1) {
        label = "false positives";
      } else if (category === "falsepositive") {
        label = "false positive";
      } else if (category === "confirmed") {
        label = "confirmed SRKW";
      } else if (category === "unknown") {
        label = "confirmed other";
      }

      return (
        <div key={`${category}`}>
          {count} {label}
        </div>
      );
    })
    .filter((c) => c); // filters out the null items

  machineItems.unshift(<strong key="machine-items">Machine</strong>);

  const machineItemString = machineItems.flatMap((item, index) =>
    index < machineItems.length - 1
      ? [item, <span key={`dot-${index}`}> • </span>]
      : [item],
  );

  return (
    <>
      <h3 style={{ marginTop: "8px" }}>
        Last 7 days <span style={{ margin: "0 4px" }}>·</span>{" "}
        <span style={{ fontWeight: "normal" }}>
          {combinedThisFeed.length} detections
        </span>
      </h3>
      <Box sx={{ mb: 3, mt: -0.75 }}>
        <div
          style={{
            lineHeight: 1.6,
            marginBottom: "12px",
            display: "flex",
            columnGap: "8px",
            flexWrap: "wrap",
          }}
        >
          {humanItemString}
        </div>
        <div
          style={{
            lineHeight: 1.6,
            marginBottom: "18px",
            display: "flex",
            columnGap: "8px",
            flexWrap: "wrap",
          }}
        >
          {machineItemString}
        </div>
      </Box>
      <Button
        href={`/reports`}
        variant="contained"
        sx={{
          mb: 3,
        }}
      >
        View all reports
      </Button>
    </>
  );
}
