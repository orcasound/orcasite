import { Box } from "@mui/material";
import { BarChart } from "@mui/x-charts/BarChart";
import React from "react";

import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";
import { oneDay } from "@/utils/masterDataHelpers";

type ChartData = {
  tick: number;
  milliseconds: number;
  label: string;
  detections: number;
  whale: number;
  vessel: number;
  other: number;
  "whale (ai)": number;
  sighting: number;
  [key: string]: number | string;
};

export default function ReportsBarChart({
  showLegend = true,
  showYAxis = true,
  showXAxis = true,
  feed,
}: {
  showLegend?: boolean;
  showYAxis?: boolean;
  showXAxis?: boolean;
  feed?: Feed;
}) {
  const msPerHour = 1000 * 60 * 60;
  const msPerDay = msPerHour * 24;

  const chartSetting = {
    yAxis: [
      {
        label: "Reports",
        dataKey: "detections",
      },
    ],
    height: showYAxis ? 230 : 100,
    margin: {
      top: showXAxis ? 10 : 0,
      right: showYAxis ? 20 : 0,
      bottom: showYAxis ? 110 : 0,
      left: showYAxis ? 40 : 0,
    },
  };

  const { filteredData, filters } = useData();
  const timeRange = filters.timeRange;

  const detections = feed
    ? filteredData.filter((d) => d.feedId === feed.id)
    : [...filteredData];

  const timeInterval = timeRange <= oneDay ? "hours" : "days";

  const feedNames = [...new Set(detections.map((el) => el.hydrophone))];
  const categories = [...new Set(detections.map((el) => el.newCategory))];
  const legend = filters.chartLegend;

  const max = Date.now();
  const min = max - timeRange;
  const startHour = new Date(min).setMinutes(0, 0, 0);
  const endHour = new Date(max).setMinutes(0, 0, 0);
  const timeDifferenceHours = (endHour - startHour) / msPerHour;
  const startDay = new Date(min).setHours(0, 0, 0);
  const endDay = new Date(max).setHours(0, 0, 0);
  const timeDifferenceDays = (endDay - startDay) / msPerDay;

  const chartData: ChartData[] = [];
  const ticks =
    timeInterval === "hours" ? timeDifferenceHours : timeDifferenceDays;
  const milliseconds = (tick: number) => {
    if (timeInterval === "hours") {
      return tick * msPerHour;
    } else {
      return tick * msPerDay;
    }
  };
  const label = (tick: number) => {
    if (timeInterval === "hours") {
      return new Date(startHour + tick * msPerHour).toLocaleTimeString();
    } else {
      return new Date(startDay + tick * msPerDay).toLocaleDateString();
    }
  };

  for (let i = 0; i < ticks; i++) {
    chartData.push({
      tick: i,
      milliseconds: milliseconds(i),
      label: label(i),
      detections: 0,
      whale: 0,
      vessel: 0,
      other: 0,
      "whale (ai)": 0,
      sighting: 0,
    });
  }

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
    for (let i = 0; i < detections.length; i++) {
      const timestamp = Date.parse(detections[i].timestampString);
      const tick = Math.round(
        (timestamp - min) / (timeInterval === "hours" ? msPerHour : msPerDay),
      );
      // iterate over each hour of the chartData array and determine if the report tick matches the chart hour
      for (let j = 0; j < chartData.length; j++) {
        if (chartData[j].tick === tick) {
          // if so, add to the count of detections, whale, vessel, other, and whale (ai)
          const chartItem = chartData[j];
          chartItem.detections += 1;

          const category = detections[i].newCategory.toLowerCase();

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
            case "sighting":
              chartItem.sighting += 1;
              break;
            default:
              break;
          }
          // also, get the hydrophone name
          const feedName = detections[i].hydrophone.toLowerCase();
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

  return (
    <Box
      sx={{
        width: "100%",
        overflowX: "auto",
      }}
    >
      <BarChart
        dataset={chartData}
        className={`${!showYAxis ? "hide-y-axis" : ""} ${!showXAxis ? "hide-x-axis" : ""}`}
        xAxis={[
          {
            scaleType: "band",
            dataKey: "label",
            label: timeInterval === "hours" ? "Hours" : "Days",
          },
        ]}
        slotProps={{
          legend: showLegend
            ? {
                position: { horizontal: "middle", vertical: "bottom" },
                padding: { bottom: 0, top: 0 },
              }
            : { hidden: true },
        }}
        series={legend === "category" ? categorySeries : hydrophoneSeries}
        {...chartSetting}
      />
      <style jsx global>{`
        .hide-y-axis .MuiChartsAxis-left {
          display: none !important;
        }

        .hide-x-axis .MuiChartsAxis-tickContainer {
          display: none !important;
        }
        .hide-x-axis .MuiChartsAxis-line {
          stroke: rgba(255, 255, 255, 0.33) !important;
        }
      `}</style>
    </Box>
  );
}
