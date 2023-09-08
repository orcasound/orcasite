import {
  Box,
  Container,
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
import { useState } from "react";

import Header from "@/components/Header";
import {
  CandidateSortField,
  SortOrder,
  useCandidatesQuery,
} from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { formatTimestamp } from "@/utils/time";

const DetectionsPage: NextPageWithLayout = () => {
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [page, setPage] = useState(0);

  const candidatesQuery = useCandidatesQuery({
    limit: rowsPerPage,
    offset: page * rowsPerPage,
    sort: [{ field: CandidateSortField.MinTime, order: SortOrder.Desc }],
  });
  const candidates = candidatesQuery?.data?.candidates?.results ?? [];
  // console.log("candidatesQuery", candidatesQuery);

  return (
    <div>
      <Head>
        <title>Detections | Orcasound</title>
      </Head>

      <main>
        <Box
          sx={{
            // use `dvh` for dynamic viewport height to handle mobile browser weirdness
            // but fallback to `vh` for browsers that don't support `dvh`
            // `&` is a workaround because sx prop can't have identical keys
            "&": {
              height: "100dvh",
            },
            height: "100vh",
            display: "flex",
            flexDirection: "column",
          }}
        >
          <Header />
          <Box sx={{ flexGrow: 1, display: "flex" }}>
            <Container>
              <h1>Detections</h1>

              <Paper elevation={1}>
                {/* <Modal
                  open={detectionModalOpen}
                  onClose={this.handleModalClose}
                  className="p-4"
                >
                  <Paper classes={{ root: "p-5" }}>
                    <Detections detections={detections} feed={feed} /> 
                  </Paper>
                </Modal> */}
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell>ID</TableCell>
                      <TableCell>Node</TableCell>
                      <TableCell align="right">Detections</TableCell>
                      <TableCell align="right">Timestamp</TableCell>
                      <TableCell align="right">Descriptions</TableCell>
                      <TableCell align="center">Actions</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {candidates.map((candidate) => (
                      <TableRow key={candidate.id} hover={true}>
                        <TableCell>{candidate.id}</TableCell>
                        <TableCell sx={{ whiteSpace: "nowrap" }}>
                          {candidate.feed?.slug}
                        </TableCell>
                        <TableCell align="right">
                          {candidate.detectionCount}
                        </TableCell>
                        <TableCell
                          align="right"
                          title={candidate.minTime?.toString() || ""}
                          sx={{ whiteSpace: "nowrap" }}
                        >
                          {candidate.minTime &&
                            formatTimestamp(candidate.minTime)}
                        </TableCell>
                        <TableCell
                          align="right"
                          title={candidate.minTime?.toString() || ""}
                        >
                          {candidate.detections
                            .map((d) => d.description)
                            .filter(
                              (d) => typeof d !== "undefined" && d !== null,
                            )
                            .slice(0, 3)
                            .join(", ")}
                        </TableCell>
                        <TableCell align="center">
                          {/* <Button
                            onClick={this.handleCandidateClick(candidate)}
                          >
                            View
                          </Button> */}
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
                            console.log(e.target.value);
                            setRowsPerPage(Number(e.target.value));
                          }}
                          rowsPerPageOptions={[10, 25, 50, 100]}
                        />
                      </TableRow>
                    </TableFooter>
                  )}
                </Table>
              </Paper>
            </Container>
          </Box>
        </Box>
      </main>
    </div>
  );
};

export default DetectionsPage;
