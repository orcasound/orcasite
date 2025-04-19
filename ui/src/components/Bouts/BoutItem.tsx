import { Box, Card, Chip, Link, Typography } from "@mui/material";
import {
  differenceInMilliseconds,
  format,
  formatDuration,
  intervalToDuration,
} from "date-fns";
import { useEffect, useState } from "react";

import {
  Bout,
  DetectionCategory,
  Feed,
  useDetectionsCountQuery,
} from "@/graphql/generated";

import CategoryIcon from "./CategoryIcon";

export default function BoutItem({
  bout,
}: {
  bout: Pick<
    Bout,
    "id" | "name" | "endTime" | "startTime" | "category" | "duration"
  > & {
    feed?: Pick<Feed, "id" | "name"> | null;
  };
}) {
  const isLive = bout.endTime === undefined || bout.endTime === null;
  const startTime = new Date(bout.startTime);

  const categoryMap = {
    BIOPHONY: "WHALE",
    ANTHROPHONY: "VESSEL",
    GEOPHONY: "OTHER",
  };

  const detectionsCount =
    useDetectionsCountQuery(
      {
        feedId: bout.feed?.id ?? "",
        fromTime: bout.startTime,
        toTime: bout.endTime,
        category: categoryMap[bout.category] as DetectionCategory,
      },
      { enabled: !!bout.feed?.id },
    ).data?.feedDetectionsCount ?? "-";

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
        <Box display="flex" width="100%" gap={3} flexWrap="wrap">
          <Box display="flex" flexDirection={"column"} gap={1} width={80}>
            <Box
              position="relative"
              flexGrow={1}
              display="flex"
              alignItems="center"
              justifyContent="center"
            >
              <CategoryIcon audioCategory={bout.category} />
            </Box>
            <Typography
              textAlign="center"
              variant="subtitle1"
              sx={{
                fontSize:
                  bout.category === "ANTHROPHONY" ? "0.75rem" : "inherit",
              }}
            >
              {bout.category}
            </Typography>
          </Box>
          <Box display="flex" flexDirection={"column"}>
            <Typography variant="h5">{bout.name ?? bout.feed?.name}</Typography>
            <Typography variant="subtitle1" sx={{ fontWeight: "normal" }}>
              {format(startTime, "h:mm:ss a O")}
              {bout.endTime &&
                ` — ${format(new Date(bout.endTime), "h:mm:ss a O")}`}
            </Typography>
            <Typography variant="subtitle2">
              {startTime.toLocaleDateString()}
            </Typography>
          </Box>

          <Box
            display="flex"
            flexDirection={{ sm: "column" }}
            sx={{
              justifyContent: "flex-start",
              alignItems: { xs: "center", sm: "flex-end" },
              ml: { sm: "auto" },
              gap: { xs: 2, sm: 0 },
              width: { xs: "100%", sm: "auto" },
            }}
          >
            {isLive && (
              <Chip
                label="LIVE"
                size="small"
                color="primary"
                sx={{ width: 64, ml: { sm: "auto" } }}
              />
            )}

            <Typography variant="overline" textAlign="right">
              {detectionsCount} detection{detectionsCount === 1 ? "" : "s"}
            </Typography>
            {duration && (
              <Box
                sx={{ mt: { sm: "auto" }, ml: { xs: "auto", sm: 0 } }}
                title={formatDuration(
                  intervalToDuration({ start: 0, end: duration }),
                )}
              >
                <Typography variant="monospace" textAlign="right">
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
  const hours = Math.floor(durationMs / 1000 / 60 / 60);

  const zeroPad = (num: number) => String(num).padStart(2, "0");
  const formatted = [hours, duration.minutes ?? 0, duration.seconds ?? 0]
    .map(zeroPad)
    .join(":");

  return formatted;
}
