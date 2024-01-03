import {
  Box,
  Button,
  Chip,
  Modal,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableFooter,
  TableHead,
  TablePagination,
  TableRow,
  Typography,
} from "@mui/material";
import Head from "next/head";
import Link from "next/link";
import { useRouter, useSearchParams } from "next/navigation";
import { useEffect, useMemo, useState } from "react";

import DetectionsTable from "@/components/DetectionsTable";
import { getReportsLayout } from "@/components/layouts/ReportsLayout";
import {
  CandidatesQuery,
  useCandidatesQuery,
  useGetCurrentUserQuery,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { analytics } from "@/utils/analytics";
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
  const router = useRouter();
  const [rowsPerPage, setRowsPerPage] = useState(50);
  const [page, setPage] = useState(0);

  const searchParams = useSearchParams();

  const candidateIdParam = searchParams.get("candidateId");

  const { currentUser } = useGetCurrentUserQuery().data ?? {};

  const [detectionModalOpen, setDetectionModalOpen] = useState(false);
  const [selectedCandidate, setSelectedCandidate] =
    useState<CandidateQueryResult>();

  // TODO: Filter by feed
  const candidatesQuery = useCandidatesQuery({
    limit: rowsPerPage,
    offset: page * rowsPerPage,
    sort: [{ field: "MIN_TIME", order: "DESC" }],
  });

  const candidates = useMemo(
    () => candidatesQuery?.data?.candidates?.results ?? [],
    [candidatesQuery?.data?.candidates?.results],
  );

  useEffect(() => {
    if (candidateIdParam) {
      const candidate = candidates.find((c) => c.id === candidateIdParam);
      if (candidate) {
        setSelectedCandidate(candidate);
        setDetectionModalOpen(true);
        analytics.reports.reportOpened(candidateIdParam);
      }
    }
  }, [candidates, candidateIdParam]);

  return (
    <div>
      <Head>
        <title>Reports | Orcasound</title>
      </Head>

      <main>
        <h1>Reports</h1>

        <Paper elevation={1} sx={{ overflow: "auto" }}>
          <Modal
            open={detectionModalOpen}
            onClose={() => {
              setDetectionModalOpen(false);
              router.back();
            }}
            className="p-4"
          >
            <Box p={4}>
              <Paper sx={{ overflow: "auto" }}>
                <Box p={5}>
                  <Box display="flex" justifyContent="space-between">
                    <Box>
                      <Typography variant="h4">Detections</Typography>
                      <Typography variant="body2">
                        Candidate {selectedCandidate?.id}
                      </Typography>
                    </Box>
                    <Box>
                      {currentUser?.moderator && selectedCandidate && (
                        <Chip
                          variant="outlined"
                          label={
                            selectedCandidate?.visible ? "Visible" : "Hidden"
                          }
                        />
                      )}
                    </Box>
                  </Box>
                  {selectedCandidate && (
                    <DetectionsTable
                      detections={selectedCandidate.detections}
                      feed={selectedCandidate.feed}
                      candidate={selectedCandidate}
                      onDetectionUpdate={() => {
                        candidatesQuery.refetch();
                      }}
                    />
                  )}
                </Box>
              </Paper>
            </Box>
          </Modal>
          <Table>
            <TableHead>
              <TableRow>
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
              {candidates.map((candidate) => (
                <TableRow
                  key={candidate.id}
                  hover={true}
                  sx={{ ...(!candidate.visible && { opacity: 0.5 }) }}
                >
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
                  <TableCell title={candidate.minTime.toString()}>
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
                      href={`/reports/?candidateId=${candidate.id}`}
                      as={`/reports/${candidate.id}`}
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
                  <TablePagination
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

DetectionsPage.getLayout = getReportsLayout;

export default DetectionsPage;
