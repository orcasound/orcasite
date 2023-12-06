import { Box, Breadcrumbs, Link, Paper, Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";

import DetectionsTable from "@/components/DetectionsTable";
import { getReportsLayout } from "@/components/layouts/ReportsLayout";
import { useCandidateQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { analytics } from "@/utils/analytics";

const CandidatePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { candidateId } = router.query;

  const candidatesQuery = useCandidateQuery({
    id: (candidateId || "") as string,
  });
  const candidate = candidatesQuery.data?.candidate;

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
            <Typography variant="h4">Detections</Typography>
            <Typography sx={{ marginBottom: 5 }} variant="body2">
              {candidate?.id}
            </Typography>
            {candidate && (
              <DetectionsTable
                detections={candidate.detections}
                feed={candidate.feed}
                candidate={candidate}
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
