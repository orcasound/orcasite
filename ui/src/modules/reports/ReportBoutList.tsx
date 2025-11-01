import {
  Box,
  Button,
  Chip,
  Link,
  Skeleton,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  Typography,
} from "@mui/material";
import {
  differenceInMilliseconds,
  format,
  formatDuration,
  intervalToDuration,
} from "date-fns";
import _ from "lodash";
import { useEffect, useState } from "react";

import {
  BoutQuery,
  DetectionCategory,
  Feed,
  useBoutsQuery,
} from "@/graphql/generated";
import { detectionCategoryToAudioCategory } from "@/utils/enum";
import { durationString } from "@/utils/time";

type BoutResult = NonNullable<BoutQuery["bout"]>;

export function ReportBoutList({
  feed,
  minTime,
  maxTime,
  category,
}: {
  feed: Pick<Feed, "id" | "name">;
  minTime: Date;
  maxTime: Date;
  category: DetectionCategory;
}) {
  console.log("feed id?", feed.id);
  const { data: boutData, isLoading } = useBoutsQuery({
    feedId: feed.id,
    filter: {
      category: { eq: detectionCategoryToAudioCategory(category) },
      startTime: { lessThanOrEqual: maxTime },
      endTime: { greaterThanOrEqual: minTime },
    },
  });

  const bouts = boutData?.bouts;

  return (
    <Box sx={{ marginTop: 10 }}>
      <h3>Bouts</h3>
      <Table sx={{ marginTop: 2 }}>
        <TableHead>
          <TableRow>
            <TableCell>#</TableCell>
            <TableCell>ID</TableCell>
            <TableCell>Name</TableCell>
            <TableCell>Category</TableCell>
            <TableCell>Start time</TableCell>
            <TableCell>End time</TableCell>
            <TableCell>Duration</TableCell>
            <TableCell>Actions</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {isLoading && (
            <TableRow>
              {_.range(8).map((_, idx) => (
                <TableCell key={idx}>
                  <Skeleton animation="wave" variant="text" />
                </TableCell>
              ))}
            </TableRow>
          )}
          {!isLoading &&
            (bouts?.count || 0) > 0 &&
            bouts?.results?.map((bout, idx) => (
              <BoutItem bout={bout} index={idx} key={bout.id} feed={feed} />
            ))}
          {!isLoading && (bouts?.count || 0) === 0 && (
            <TableRow>
              <TableCell colSpan={8} sx={{ textAlign: "center" }}>
                No bouts for this candidate
              </TableCell>
            </TableRow>
          )}
        </TableBody>
      </Table>
    </Box>
  );
}

function BoutItem({
  bout,
  feed,
  index,
}: {
  bout: BoutResult;
  feed: Pick<Feed, "name">;
  index: number;
}) {
  const [duration, setDuration] = useState(
    bout.duration && bout.duration * 1000,
  );
  const isLive = bout.endTime === undefined || bout.endTime === null;
  useEffect(() => {
    let interval: NodeJS.Timeout | undefined;
    if (isLive) {
      setDuration(
        differenceInMilliseconds(new Date(), new Date(bout.startTime)),
      );
      interval = setInterval(() => {
        setDuration(
          differenceInMilliseconds(new Date(), new Date(bout.startTime)),
        );
      }, 1000);
    }

    return () => {
      clearInterval(interval);
    };
  }, [bout.startTime, isLive]);

  return (
    <TableRow key={bout.id}>
      <TableCell>{index + 1}</TableCell>
      <TableCell>{bout.id}</TableCell>
      <TableCell>{bout.name || feed.name}</TableCell>
      <TableCell>
        <Chip label={bout.category} />
      </TableCell>
      <TableCell>
        {format(new Date(bout.startTime), "h:mm:ss a O")}
        <Typography fontSize={"small"}>
          {new Date(bout.startTime).toLocaleDateString()}
        </Typography>
      </TableCell>
      <TableCell>
        {bout.endTime && (
          <>
            {format(new Date(bout.endTime), "h:mm:ss a O")}
            <Typography fontSize={"small"}>
              {new Date(bout.endTime).toLocaleDateString()}
            </Typography>
          </>
        )}
      </TableCell>
      <TableCell>
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
      </TableCell>
      <TableCell>
        <Link href={`/bouts/${bout.id}`} title={`/bouts/${bout.id}`}>
          <Button size="small">View</Button>
        </Link>
      </TableCell>
    </TableRow>
  );
}
