import {
  Button,
  Chip,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableFooter,
  TableHead,
  TableRow,
} from "@mui/material";
import Head from "next/head";
import Link from "next/link";
import { useMemo, useState } from "react";

import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import TablePaginationWrapper from "@/components/TablePaginationWrapper";
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
      if (category && category in counts) {
        counts[category] += 1;
      } else if (category) {
        counts[category] = 1;
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

  const candidates = useMemo(
    () => candidatesQuery?.data?.candidates?.results ?? [],
    [candidatesQuery?.data?.candidates?.results],
  );
  return (
    <div>
      <Head>
        <title>Reports | Orcasound</title>
      </Head>

      <main>
        <h1>Reports</h1>

        <Paper elevation={1} sx={{ overflow: "auto" }}>
          {candidatesQuery.isSuccess && (
            <TablePaginationWrapper
              component="div"
              count={candidatesQuery.data?.candidates?.count || 0}
              page={page}
              rowsPerPage={rowsPerPage}
              onPageChange={(e, pg) => {
                setPage(pg);
              }}
              onRowsPerPageChange={(e) => {
                setRowsPerPage(Number(e.target.value));
              }}
              rowsPerPageOptions={[10, 50, 100, 1000]}
              showTopBorder={true}
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
                    {Object.entries(getCategoryCounts(candidate))
                      .map(
                        ([category, count]) =>
                          `${category.toLowerCase()} [${count}]`,
                      )
                      .join(", ")}{" "}
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
                  <TablePaginationWrapper
                    count={candidatesQuery.data?.candidates?.count || 0}
                    page={page}
                    rowsPerPage={rowsPerPage}
                    onPageChange={(e, pg) => {
                      setPage(pg);
                    }}
                    onRowsPerPageChange={(e) => {
                      setRowsPerPage(Number(e.target.value));
                    }}
                    rowsPerPageOptions={[10, 50, 100, 1000]}
                  />
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
