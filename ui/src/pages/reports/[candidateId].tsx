import { Add } from "@mui/icons-material";
import {
  Box,
  Breadcrumbs,
  Button,
  Chip,
  Link,
  Paper,
  Typography,
} from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";

import DetectionsTable from "@/components/DetectionsTable";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import { useCandidateQuery, useGetCurrentUserQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { analytics } from "@/utils/analytics";

const CandidatePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { candidateId } = router.query;

  const candidateQuery = useCandidateQuery({
    id: (candidateId || "") as string,
  });
  const candidate = candidateQuery.data?.candidate;
  const { currentUser } = useGetCurrentUserQuery().data ?? {};

  if (candidateId && typeof candidateId === "string") {
    analytics.reports.reportOpened(candidateId);
  }

  const time = candidate && new Date(candidate.minTime).toISOString();
  const categoryMap = {
    WHALE: "biophony",
    VESSEL: "anthrophony",
    OTHER: "geophony",
  };
  const category = candidate?.category && categoryMap[candidate.category];

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

        <Paper sx={{ marginTop: 4, overflow: "auto" }}>
          <Box p={5}>
            <Box display="flex" justifyContent="space-between" mb={5}>
              <Box display="flex" flexDirection="column" gap={1}>
                <Typography variant="h4">Detections</Typography>
                <Box display="flex" gap={1} alignItems="center">
                  {currentUser?.moderator && (
                    <Chip
                      variant="outlined"
                      label={candidate?.visible ? "Visible" : "Hidden"}
                    />
                  )}
                  <Typography variant="body2">{candidate?.id}</Typography>
                </Box>
              </Box>
              <Box display="flex" gap={1}>
                {candidate && currentUser?.moderator && (
                  <Link
                    href={`/bouts/new/${candidate.feed.slug}?time=${time}&category=${category}`}
                  >
                    <Button size="small" variant="outlined" startIcon={<Add />}>
                      New bout
                    </Button>
                  </Link>
                )}
              </Box>
            </Box>
            {candidate && (
              <DetectionsTable
                detections={candidate.detections}
                feed={candidate.feed}
                candidate={candidate}
                onDetectionUpdate={() => {
                  candidateQuery.refetch();
                }}
              />
            )}
          </Box>
        </Paper>
      </main>
    </div>
  );
};

CandidatePage.getLayout = getSimpleLayout;

export default CandidatePage;
