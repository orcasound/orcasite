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
  feedList,
}: {
  dataset: CombinedData[];
  timeRange: number;
  feedList: Feed[];
}) {
  const [legend, setLegend] = React.useState(true);

  const max = Date.now();
  const min = max - timeRange;
  const startHour = new Date(min).setMinutes(0, 0, 0);
  const endHour = new Date(max).setMinutes(0, 0, 0);
  const timeDifferenceHours = (endHour - startHour) / (1000 * 60 * 60);

  const chartData: ChartData[] = [
    {
      tick: 0,
      milliseconds: 0,
      label: "",
      detections: 0,
      whale: 0,
      vessel: 0,
      other: 0,
      "whale (ai)": 0,
    },
  ];

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

  const hydrophoneSeries = feedList.map((el: Feed) => ({
    dataKey: el.name.toLowerCase(),
    label: el.name,
  }));
  hydrophoneSeries.shift(); // remove the "all hydrophones" from legend

  const hydrophoneCounts = feedList.map((el) => ({
    [el.name.toLowerCase()]: 0,
  }));

  chartData.forEach((el) => {
    hydrophoneCounts.forEach((hydro) => {
      Object.assign(el, hydro);
    });
  });

  const countData = () => {
    for (let i = 0; i < dataset.length; i++) {
      const timestamp = Date.parse(dataset[i].dateString);
      const tick = Math.round((timestamp - min) / (1000 * 60 * 60));
      for (let j = 0; j < chartData.length; j++) {
        if (chartData[j].tick === tick) {
          chartData[j].detections += 1;
          chartData[j][
            dataset[i].newCategory.toLowerCase() as keyof ChartData
          ] += 1;
          chartData[j][
            dataset[i].hydrophone.toLowerCase() as keyof ChartData
          ] += 1;
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
    button.name === "category" ? setLegend(true) : setLegend(false);
  };

  return (
    <>
      <Box style={{ display: "flex", gap: 16, padding: "24px 0" }}>
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
      <BarChart
        dataset={chartData}
        xAxis={[{ scaleType: "band", dataKey: "label", label: "Hour" }]}
        slotProps={{
          legend: {
            position: { horizontal: "middle", vertical: "top" },
            padding: { bottom: 20, top: 5 },
          },
        }}
        series={legend ? categorySeries : hydrophoneSeries}
        {...chartSetting}
      />
    </>
  );
}
