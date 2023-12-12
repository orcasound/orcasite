import { Box, Breadcrumbs, Chip, Link, Paper, Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";

import DetectionsTable from "@/components/DetectionsTable";
import { getReportsLayout } from "@/components/layouts/ReportsLayout";
import { useCandidateQuery, useGetCurrentUserQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { analytics } from "@/utils/analytics";

const CandidatePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { candidateId } = router.query;

  const candidatesQuery = useCandidateQuery({
    id: (candidateId || "") as string,
  });
  const candidate = candidatesQuery.data?.candidate;
  const { currentUser } = useGetCurrentUserQuery().data ?? {};

  if (candidateId && typeof candidateId === "string") {
    analytics.reports.reportOpened(candidateId);
  }

  return (
    <div>
      <Head>Report {candidateId} | Orcasound </Head>

      <main>
        <Breadcrumbs sx={{ paddingTop: 4 }}>
          <Link underline="hover" color="inherit" href={"/reports"}>
            Reports
          </Link>
          <Typography>{candidate?.id}</Typography>
        </Breadcrumbs>

        <Paper sx={{ marginTop: 4 }}>
          <Box p={5}>
            <Box display="flex" justifyContent="space-between">
              <Box>
                <Typography variant="h4">Detections</Typography>
                <Typography sx={{ marginBottom: 5 }} variant="body2">
                  {candidate?.id}
                </Typography>
              </Box>
              <Box>
                {currentUser?.moderator && (
                  <Chip
                    variant="outlined"
                    label={candidate?.visible ? "Visible" : "Hidden"}
                  />
                )}
              </Box>
            </Box>
            {candidate && (
              <DetectionsTable
                detections={candidate.detections}
                feed={candidate.feed}
                candidate={candidate}
                onDetectionUpdate={() => {
                  candidatesQuery.refetch();
                }}
              />
            )}
          </Box>
        </Paper>
      </main>
    </div>
  );
};

CandidatePage.getLayout = getReportsLayout;

export default CandidatePage;
