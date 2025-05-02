import { Box } from "@mui/material";
import { BarChart } from "@mui/x-charts/BarChart";
import React, { useEffect } from "react";

import { useData } from "@/context/DataContext";

const chartSetting = {
  yAxis: [
    {
      label: "Reports",
      dataKey: "detections",
    },
  ],
  height: 340,
  margin: {
    top: 10,
    right: 20,
    bottom: 110,
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
  [key: string]: number | string;
};

export default function ReportsBarChart() {
  const { feeds, filteredData, filters } = useData();
  const timeRange = filters.timeRange;

  const feedNames = [...new Set(filteredData.map((el) => el.hydrophone))];
  const categories = [...new Set(filteredData.map((el) => el.newCategory))];
  const legend = filters.chartLegend;

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

  // const categorySeries = [
  //   { dataKey: "whale", label: "Whale" },
  //   { dataKey: "vessel", label: "Vessel" },
  //   { dataKey: "other", label: "Other" },
  //   { dataKey: "whale (ai)", label: "Whale (AI)" },
  // ];

  const categorySeries = categories.map((el) => ({
    dataKey: el.toLowerCase(),
    label: el,
  }));

  const hydrophoneSeries = feedNames.map((el) => ({
    dataKey: el.toLowerCase(),
    label: el,
  }));

  // create an array of objects for each hydrophone name that looks like [{"name1": 0}, {"name2": 0}]
  const hydrophoneCounts = feedNames.map((el) => ({
    [el.toLowerCase()]: 0,
  }));

  // add each name/value pair in hydrophoneCounts to each item in chartData
  chartData.forEach((el) => {
    hydrophoneCounts.forEach((hydro) => {
      Object.assign(el, hydro);
    });
  });

  const countData = () => {
    // iterate over each report in the filteredData array and define the timestamp and tick index
    for (let i = 0; i < filteredData.length; i++) {
      const timestamp = Date.parse(filteredData[i].timestampString);
      const tick = Math.round((timestamp - min) / (1000 * 60 * 60));
      // iterate over each hour of the chartData array and determine if the report tick matches the chart hour
      for (let j = 0; j < chartData.length; j++) {
        if (chartData[j].tick === tick) {
          // if so, add to the count of detections, whale, vessel, other, and whale (ai)
          const chartItem = chartData[j];
          chartItem.detections += 1;

          const category = filteredData[i].newCategory.toLowerCase();

          switch (category) {
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
          // also, get the hydrophone name
          const feedName = filteredData[i].hydrophone.toLowerCase();
          // if the chartItem object has that feedName as a key, increment it by one
          if (chartItem[feedName] !== undefined) {
            if (typeof chartItem[feedName] === "number") {
              chartItem[feedName] += 1;
            }
          }
        }
      }
    }
  };
  countData();

  useEffect(() => {
    console.log(JSON.stringify(filteredData[0], null, 2));
    console.log(feedNames);
  });

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
            position: { horizontal: "middle", vertical: "bottom" },
            padding: { bottom: 0, top: 0 },
          },
        }}
        series={legend === "category" ? categorySeries : hydrophoneSeries}
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
        {/* <ChartButton
          onClick={handleLegend}
          name="category"
          label="By category"
        />
        <ChartButton
          onClick={handleLegend}
          name="hydro"
          label="By hydrophone"
        /> */}
      </Box>
    </Box>
  );
}
