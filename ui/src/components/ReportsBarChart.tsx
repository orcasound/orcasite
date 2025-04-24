import { Box, Button } from "@mui/material";
import { BarChart } from "@mui/x-charts/BarChart";
import React from "react";

import { Feed } from "@/graphql/generated";
import { CombinedData } from "@/types/DataTypes";

const chartSetting = {
  yAxis: [
    {
      label: "Reports",
      dataKey: "detections",
    },
  ],
  height: 300,
  margin: {
    top: 70,
    right: 20,
    bottom: 40,
    left: 40,
  },
};

type ChartData = {
  tick: number;
  milliseconds: number;
  label: string;
  detections: number;
  whale: number;
  vessel: number;
  other: number;
  "whale (ai)": number;
};

export default function ReportsBarChart({
  dataset,
  timeRange,
  feeds,
}: {
  dataset: CombinedData[];
  timeRange: number;
  feeds: Feed[];
}) {
  // // get hydrophone feed list
  // const feedsQueryResult = useFeedsQuery();
  // const feeds = feedsQueryResult.data?.feeds ?? [];

  const [legend, setLegend] = React.useState(true);

  const max = Date.now();
  const min = max - timeRange;
  const startHour = new Date(min).setMinutes(0, 0, 0);
  const endHour = new Date(max).setMinutes(0, 0, 0);
  const timeDifferenceHours = (endHour - startHour) / (1000 * 60 * 60);

  const chartData: ChartData[] = [];

  for (let i = 0; i < timeDifferenceHours; i++) {
    chartData.push({
      tick: i,
      milliseconds: i * 1000 * 60 * 60,
      label: new Date(startHour + i * 1000 * 60 * 60).toLocaleString(),
      detections: 0,
      whale: 0,
      vessel: 0,
      other: 0,
      "whale (ai)": 0,
    });
  }

  const categorySeries = [
    { dataKey: "whale", label: "Whale" },
    { dataKey: "vessel", label: "Vessel" },
    { dataKey: "other", label: "Other" },
    { dataKey: "whale (ai)", label: "Whale (AI)" },
  ];

  const hydrophoneSeries = feeds.map((el) => ({
    dataKey: el.name.toLowerCase(),
    label: el.name,
  }));
  hydrophoneSeries.shift(); // remove the "all hydrophones" from legend

  const hydrophoneCounts = feeds.map((el) => ({
    [el.name.toLowerCase()]: 0,
  }));

  chartData.forEach((el) => {
    hydrophoneCounts.forEach((hydro) => {
      Object.assign(el, hydro);
    });
  });

  const countData = () => {
    for (let i = 0; i < dataset.length; i++) {
      const timestamp = Date.parse(dataset[i].timestampString);
      const tick = Math.round((timestamp - min) / (1000 * 60 * 60));
      for (let j = 0; j < chartData.length; j++) {
        if (chartData[j].tick === tick) {
          const chartItem = chartData[j];
          chartItem.detections += 1;
          if (dataset[i].newCategory.toLowerCase() === "whale") {
            chartItem.whale += 1;
          }
          switch (dataset[i].newCategory.toLowerCase()) {
            case "whale":
              chartItem.whale += 1;
              break;
            case "vessel":
              chartItem.vessel += 1;
              break;
            case "other":
              chartItem.other += 1;
              break;
            case "whale (ai)":
              chartItem["whale (ai)"] += 1;
              break;
            default:
              break;
          }
        }
      }
    }
  };
  countData();

  interface ChartButtonProps {
    onClick: (e: React.MouseEvent) => void;
    name: string;
    label: string;
  }

  const ChartButton: React.FC<ChartButtonProps> = ({
    onClick,
    name,
    label,
  }) => {
    return (
      <Button
        size="small"
        onClick={onClick}
        name={name}
        variant={
          legend && name === "category"
            ? "contained"
            : !legend && name === "hydro"
              ? "contained"
              : "outlined"
        }
      >
        {label}
      </Button>
    );
  };

  const handleLegend = (e: React.MouseEvent) => {
    const button = e.target as HTMLButtonElement;
    setLegend(button.name === "category"); // returns true/false
  };

  return (
    <Box
      sx={{
        width: "100%",
        overflowX: "auto",
      }}
    >
      <BarChart
        dataset={chartData}
        xAxis={[{ scaleType: "band", dataKey: "label", label: "Hour" }]}
        slotProps={{
          legend: {
            position: { horizontal: "middle", vertical: "top" },
            padding: { bottom: 0, top: 0 },
          },
        }}
        series={legend ? categorySeries : hydrophoneSeries}
        {...chartSetting}
      />
      <Box
        style={{
          display: "flex",
          justifyContent: "center",
          gap: 16,
          padding: "24px 0",
        }}
      >
        <ChartButton
          onClick={handleLegend}
          name="category"
          label="By category"
        />
        <ChartButton
          onClick={handleLegend}
          name="hydro"
          label="By hydrophone"
        />
      </Box>
    </Box>
  );
}
