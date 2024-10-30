import { Circle, Close, Launch, Person, ViewList } from "@mui/icons-material";
import {
  Box,
  Button,
  Card,
  Chip,
  IconButton,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
  Tooltip,
  Typography,
  useTheme,
} from "@mui/material";
import { useEffect, useMemo, useState } from "react";
import { VegaLite } from "react-vega";

import {
  DetectionCategory,
  Feed,
  useDetectionsQuery,
} from "@/graphql/generated";
import { useListenerCount } from "@/hooks/useFeedPresence";
import { formatTimestamp } from "@/utils/time";

// eslint-disable-next-line import/no-unused-modules
export default function FeedItem({
  feed,
  onStatUpdate,
}: {
  feed: Pick<Feed, "id" | "name" | "slug" | "online">;
  onStatUpdate?: (feedId: string, stat: string, value: number) => void;
}) {
  const categories: Array<DetectionCategory> = ["WHALE", "VESSEL", "OTHER"];

  const [showTable, setShowTable] = useState(false);
  const [selectedCategory, setSelectedCategory] = useState<DetectionCategory>();
  const theme = useTheme();

  const listenerCount = useListenerCount(feed.slug);

  const now = useMemo(() => new Date(), []);
  const oneHourAgo = new Date(now.valueOf() - 60 * 60 * 1000);
  const detectionQueryResult = useDetectionsQuery({
    filter: {
      feed: { slug: { eq: feed.slug } },
      timestamp: { greaterThan: oneHourAgo },
    },
    sort: { field: "TIMESTAMP", order: "DESC" },
  });

  const recentDetections = detectionQueryResult.data?.detections?.results ?? [];

  const detections =
    selectedCategory !== undefined
      ? recentDetections.filter((det) => det.category === selectedCategory)
      : recentDetections;

  const detsCount = recentDetections.length;
  const detsCount15MinAgo = recentDetections.filter(
    ({ timestamp }) => timestamp > new Date(now.valueOf() - 15 * 60 * 1000),
  ).length;
  const detsCount5MinAgo = recentDetections.filter(
    ({ timestamp }) => timestamp > new Date(now.valueOf() - 5 * 60 * 1000),
  ).length;

  const detectionChartData = useMemo(
    () =>
      recentDetections.map(({ timestamp, category }) =>
        // Time ago in minutes
        ({
          cat: category,
          minutesAgo: Math.floor(
            (now.valueOf() - timestamp.valueOf()) / (60 * 1000),
          ),
        }),
      ),

    // eslint-disable-next-line react-hooks/exhaustive-deps
    [recentDetections.length],
  );

  useEffect(() => {
    if (onStatUpdate) {
      if (listenerCount !== undefined) {
        onStatUpdate(feed.id, "listeners", listenerCount);
      }
      if (typeof detsCount === "number") {
        onStatUpdate(feed.id, "detections", detsCount);
      }
      ["whale", "vessel", "other"].forEach((cat) => {
        onStatUpdate(
          feed.id,
          cat,
          recentDetections.filter(
            ({ category }) => category?.toLocaleLowerCase() === cat,
          ).length,
        );
      });
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [
    feed.id,
    recentDetections.length,
    detsCount,
    onStatUpdate,
    listenerCount,
  ]);

  return (
    <Card sx={{ width: "100%", p: 2, overflowX: "auto" }} elevation={1}>
      {/* Feed Summary */}
      <Box
        display="flex"
        alignItems={{ sm: "center" }}
        flexWrap={"wrap"}
        flexDirection={{ xs: "column", sm: "row" }}
      >
        <Box display="flex" alignItems="center">
          <Tooltip title={feed.online ? "online" : "offline"}>
            <Circle
              sx={{
                mr: 2,
                fontSize: 12,
                color: feed.online ? theme.palette.success.main : "gray",
              }}
            />
          </Tooltip>
          <Typography variant="body1">{feed.name}</Typography>
          <IconButton
            href={`/listen/${feed.slug}`}
            size="small"
            sx={{ transform: "scale(0.8)" }}
          >
            <Launch />
          </IconButton>
        </Box>
        <Box
          sx={{
            ml: { sm: "auto" },
            mt: { xs: 2, sm: 0 },
            mb: { xs: 3, sm: 0 },
          }}
          display="flex"
          flexDirection={{ xs: "column", sm: "row" }}
          alignSelf={"flex-start"}
        >
          <Box>
            <Typography component="div" mr={3} variant="overline">
              Detections
            </Typography>
            <Box
              alignItems="center"
              display="flex"
              justifyContent="space-between"
            >
              <Typography component="div" mr={3}>
                {detsCount5MinAgo}{" "}
                <Typography
                  fontSize="small"
                  noWrap
                  fontWeight="bold"
                  color={theme.palette.accent2.main}
                >
                  5 min
                </Typography>
              </Typography>
              <Typography component="div" mr={3}>
                {detsCount15MinAgo}
                <Typography
                  fontSize="small"
                  noWrap
                  color={theme.palette.accent2.main}
                  fontWeight="bold"
                >
                  15 min
                </Typography>
              </Typography>
              <Typography component="div" mr={5}>
                {detsCount}{" "}
                <Typography
                  fontSize="small"
                  noWrap
                  color={theme.palette.accent2.main}
                  fontWeight="bold"
                >
                  1 hr
                </Typography>
              </Typography>
            </Box>
          </Box>

          <Box sx={{ my: { xs: 2, sm: 0 } }}>
            <Typography component="div" mr={3} variant="overline">
              Categories
            </Typography>
            <Box
              alignItems="center"
              display="flex"
              justifyContent="space-between"
            >
              {categories.map((cat, i) => (
                <Typography key={i} component="div" mr={3}>
                  {
                    recentDetections.filter(({ category }) => cat === category)
                      .length
                  }{" "}
                  <Typography
                    fontSize="small"
                    noWrap
                    color={theme.palette.accent2.main}
                    fontWeight="bold"
                  >
                    {cat}
                  </Typography>
                </Typography>
              ))}
            </Box>
          </Box>

          <Box alignSelf={"stretch"} sx={{ my: { xs: 2, sm: 0 } }}>
            <Typography component="div" mr={3} variant="overline">
              Listeners
            </Typography>
            <Typography display="flex" alignItems="center" fontSize={14}>
              <Person sx={{ mr: 1, color: theme.palette.accent2.main }} />
              {listenerCount === undefined ? "-" : listenerCount}
            </Typography>
          </Box>
        </Box>
      </Box>

      {/* Detections charts */}
      {detsCount > 0 && (
        <Box sx={{ mb: 3 }}>
          <Box
            display="flex"
            flexDirection={{ xs: "column", md: "row" }}
            justifyContent="space-between"
            alignItems="center"
            width="100%"
          >
            {categories.map((category) => (
              <Box
                key={category}
                m={3}
                flexGrow={1}
                width={"100%"}
                display="flex"
                alignItems="center"
                flexDirection="column"
              >
                <Typography
                  fontSize="small"
                  fontWeight="bold"
                  textAlign={{ xs: "center", sm: "left" }}
                >
                  {category}
                </Typography>
                <VegaLite
                  actions={false}
                  spec={{
                    $schema: "https://vega.github.io/schema/vega-lite/v5.json",
                    description: `Bar chart of recent ${category.toLowerCase()} detections`,
                    mark: "bar",
                    data: {
                      values: detectionChartData.filter(
                        ({ cat }) => cat === category,
                      ),
                    },
                    encoding: {
                      x: {
                        bin: { step: 5, extent: [0, 60] },
                        field: "minutesAgo",
                        type: "quantitative",
                        title: "minutes ago",
                        sort: "descending",
                      },
                      y: {
                        aggregate: "count",
                        title: "detections",
                        type: "quantitative",
                      },
                      color: { value: theme.palette.accent2.light },
                    },
                  }}
                />
              </Box>
            ))}
          </Box>
        </Box>
      )}

      {/* Detections table */}
      <Box>
        <Box display="flex">
          <Button
            variant={detsCount === 0 ? "text" : "outlined"}
            onClick={() => setShowTable(!showTable)}
            endIcon={
              detsCount === 0 ? undefined : showTable ? <Close /> : <ViewList />
            }
            disabled={detsCount === 0}
          >
            {detsCount === 0 ? "No recent " : ""}
            Detections
          </Button>

          {showTable && (
            <Box display="flex" ml={2}>
              {categories.map((category, i) => (
                <Button
                  key={i}
                  variant={selectedCategory === category ? "contained" : "text"}
                  sx={{ px: 2, mr: 1 }}
                  onClick={() =>
                    selectedCategory === category
                      ? setSelectedCategory(undefined)
                      : setSelectedCategory(category)
                  }
                >
                  {category}
                </Button>
              ))}
            </Box>
          )}
        </Box>
        {showTable && detsCount > 0 && (
          <Box sx={{ overflowX: "scroll" }}>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell>ID</TableCell>
                  <TableCell>Category</TableCell>
                  <TableCell>Description</TableCell>
                  <TableCell align="right">Timestamp</TableCell>
                  <TableCell align="right">Candidate</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {detections.map((det, index) => (
                  <TableRow key={index}>
                    <TableCell>
                      <Typography variant="caption">{det.id}</Typography>
                    </TableCell>
                    <TableCell>
                      <Chip label={det.category} />
                    </TableCell>
                    <TableCell>{det.description}</TableCell>
                    <TableCell align="right" title={det.timestamp.toString()}>
                      {formatTimestamp(det.timestamp)}
                    </TableCell>
                    <TableCell align="right">
                      {det?.candidate?.id && (
                        <IconButton
                          href={`/reports/${det?.candidate?.id}`}
                          size="small"
                          sx={{ transform: "scale(0.8)" }}
                        >
                          <Launch />
                        </IconButton>
                      )}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </Box>
        )}
      </Box>
    </Card>
  );
}
