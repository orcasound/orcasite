import {
  Button,
  Chip,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableFooter,
  TableHead,
  TablePagination,
  TableRow,
} from "@mui/material";
import Head from "next/head";
import Link from "next/link";
import { useState } from "react";

import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import {
  CandidatesQuery,
  useCandidatesQuery,
  useGetCurrentUserQuery,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { formatTimestamp } from "@/utils/time";

type CandidateQueryCandidates = NonNullable<CandidatesQuery["candidates"]>;
type CandidateQueryResult = NonNullable<CandidateQueryCandidates["results"]>[0];

const getCategoryCounts = (candidate: CandidateQueryResult) => {
  return candidate.detections.reduce(
    (counts, detection) => {
      const category = detection.category;
      if (category) {
        counts[category] = (counts[category] || 0) + 1;
      }
      return counts;
    },
    {} as Record<string, number>,
  );
};

const getSourceCounts = (candidate: CandidateQueryResult) => {
  return candidate.detections.reduce(
    (counts, detection) => {
      const source = detection.source;
      if (source) {
        counts[source] = (counts[source] || 0) + 1;
      }
      return counts;
    },
    {} as Record<string, number>,
  );
};

const DetectionsPage: NextPageWithLayout = () => {
  const [rowsPerPage, setRowsPerPage] = useState(50);
  const [page, setPage] = useState(0);

  const { currentUser } = useGetCurrentUserQuery().data ?? {};

  // TODO: Filter by feed
  const offset = page * rowsPerPage;
  const candidatesQuery = useCandidatesQuery({
    limit: rowsPerPage,
    offset: offset,
    sort: [{ field: "MIN_TIME", order: "DESC" }],
  });

  const candidatesData = candidatesQuery?.data?.candidates;
  const candidates = candidatesData?.results ?? [];
  const candidatesCount = candidatesData?.count ?? 0;

  const paginationProps = {
    count: candidatesCount,
    page: page,
    rowsPerPage: rowsPerPage,
    onPageChange: (_e: unknown, newPage: number) => setPage(newPage),
    onRowsPerPageChange: (e: React.ChangeEvent<HTMLInputElement>) => {
      setRowsPerPage(Number(e.target.value));
    },
    rowsPerPageOptions: [10, 50, 100, 1000],
  };

  return (
    <div>
      <Head>
        <title>Reports | Orcasound</title>
      </Head>

      <main>
        <h1>Reports</h1>

        <Paper elevation={1} sx={{ overflow: "auto" }}>
          {candidatesQuery.isSuccess && (
            <TablePagination
              {...paginationProps}
              component="div"
              sx={{
                borderBottom: 1,
                borderColor: "divider",
                display: "flex",
                justifyContent: "flex-end",
                alignItems: "center",
              }}
            />
          )}
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>#</TableCell>
                <TableCell>ID</TableCell>
                <TableCell>Node</TableCell>
                <TableCell align="right">Detections</TableCell>
                <TableCell align="right">Timestamp</TableCell>
                <TableCell>Categories</TableCell>
                <TableCell>Source</TableCell>
                <TableCell>Descriptions</TableCell>

                {currentUser?.moderator && <TableCell>Status</TableCell>}
                <TableCell align="center">Actions</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {candidates.map((candidate, index) => (
                <TableRow
                  key={candidate.id}
                  hover={true}
                  sx={{ ...(!candidate.visible && { opacity: 0.5 }) }}
                >
                  <TableCell>{index + offset + 1}</TableCell>
                  <TableCell>{candidate.id}</TableCell>
                  <TableCell sx={{ whiteSpace: "nowrap" }}>
                    {candidate.feed.slug}
                  </TableCell>
                  <TableCell align="right">
                    {candidate.detectionCount}
                  </TableCell>
                  <TableCell
                    align="right"
                    title={candidate.minTime.toString()}
                    sx={{ whiteSpace: "nowrap" }}
                  >
                    {formatTimestamp(candidate.minTime)}
                  </TableCell>
                  <TableCell>
                    {Object.entries(getCategoryCounts(candidate)).map(
                      ([category, count]) => (
                        <Chip
                          key={category}
                          label={`${category.toLowerCase()} ${count}`}
                          size="small"
                          sx={{
                            mb: 0.5,
                          }}
                        />
                      ),
                    )}
                  </TableCell>
                  <TableCell>
                    {Object.entries(getSourceCounts(candidate)).map(
                      ([source, count]) => (
                        <Chip
                          key={source}
                          label={`${source.toLowerCase()} ${count}`}
                          size="small"
                          variant="outlined"
                          color="primary"
                          sx={{
                            mb: 0.5,
                          }}
                        />
                      ),
                    )}
                  </TableCell>
                  <TableCell
                    title={candidate.minTime.toString()}
                    sx={{ wordWrap: "anywhere" }}
                  >
                    {candidate.detections
                      .map((d) => d.description)
                      .filter((d) => typeof d !== "undefined" && d !== null)
                      .slice(0, 3)
                      .join(", ")}
                  </TableCell>
                  {currentUser?.moderator && (
                    <TableCell>
                      <Chip
                        label={candidate.visible ? "Visible" : "Hidden"}
                        variant="outlined"
                      />
                    </TableCell>
                  )}
                  <TableCell align="center">
                    <Link
                      href={`/reports/${candidate.id}`}
                      scroll={false}
                      shallow={true}
                    >
                      <Button>View</Button>
                    </Link>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
            {candidatesQuery.isSuccess && (
              <TableFooter>
                <TableRow>
                  <TablePagination {...paginationProps} />
                </TableRow>
              </TableFooter>
            )}
          </Table>
        </Paper>
      </main>
    </div>
  );
};

DetectionsPage.getLayout = getSimpleLayout;

export default DetectionsPage;
