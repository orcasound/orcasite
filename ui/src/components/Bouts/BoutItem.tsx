import { Box, Card, Chip, Link, Typography } from "@mui/material";
import {
  differenceInMilliseconds,
  format,
  formatDuration,
  intervalToDuration,
} from "date-fns";
import { useEffect, useState } from "react";

import { Bout, Feed } from "@/graphql/generated";

import CategoryIcon from "./CategoryIcon";

export default function BoutItem({
  bout,
}: {
  bout: Pick<Bout, "id" | "endTime" | "startTime" | "category" | "duration"> & {
    feed?: Pick<Feed, "name"> | null;
  };
}) {
  const isLive = bout.endTime === undefined || bout.endTime === null;
  const startTime = new Date(bout.startTime);

  const [duration, setDuration] = useState(
    bout.duration && bout.duration * 1000,
  );

  useEffect(() => {
    let interval: NodeJS.Timeout | undefined;
    if (isLive) {
      setDuration(differenceInMilliseconds(new Date(), bout.startTime));
      interval = setInterval(() => {
        setDuration(differenceInMilliseconds(new Date(), bout.startTime));
      }, 1000);
    }

    return () => {
      clearInterval(interval);
    };
  }, [bout.startTime, isLive]);
  return (
    <Link
      href={`/bouts/${bout.id}`}
      title={`/bouts/${bout.id}`}
      sx={{ width: "100%", textDecoration: "none" }}
    >
      <Card sx={{ width: "100%", p: 3 }} elevation={1}>
        <Box display="flex" width="100%" gap={3}>
          <Box display="flex" flexDirection={"column"} gap={1}>
            <CategoryIcon audioCategory={bout.category} size={50} />
            <Typography textAlign="center" variant="subtitle1">
              {bout.category}
            </Typography>
          </Box>
          <Box display="flex" flexDirection={"column"}>
            <Typography variant="h5">{bout.feed?.name}</Typography>
            <Typography variant="subtitle1" sx={{ fontWeight: "normal" }}>
              {format(startTime, "h:mm:ss a O")}
              {bout.endTime &&
                ` — ${format(new Date(bout.endTime), "h:mm:ss a O")}`}
            </Typography>
            <Typography variant="subtitle2">
              {startTime.toLocaleDateString()}
            </Typography>
          </Box>
          <Box display="flex" flexDirection={"column"} ml="auto">
            {isLive && (
              <Chip
                label="LIVE"
                size="small"
                color="primary"
                sx={{ width: 64, ml: "auto" }}
              />
            )}
            {duration && (
              <Box
                mt="auto"
                title={formatDuration(
                  intervalToDuration({ start: 0, end: duration }),
                )}
              >
                <Typography variant="monospace">
                  {durationString(duration)}
                </Typography>
              </Box>
            )}
          </Box>
        </Box>
      </Card>
    </Link>
  );
}

function durationString(durationMs: number | null | undefined) {
  if (typeof durationMs !== "number") return "";
  const duration = intervalToDuration({ start: 0, end: durationMs });
  const zeroPad = (num: number) => String(num).padStart(2, "0");
  const daysInHours = duration.days ? duration.days * 24 : 0;

  const formatted = [
    daysInHours + (duration.hours ?? 0),
    duration.minutes ?? 0,
    duration.seconds ?? 0,
  ]
    .map(zeroPad)
    .join(":");

  return formatted;
}
