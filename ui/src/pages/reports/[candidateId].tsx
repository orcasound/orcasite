import {
  Box,
  Breadcrumbs,
  Container,
  Link,
  Paper,
  Typography,
} from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";

import DetectionsTable from "@/components/DetectionsTable";
import Header from "@/components/Header";
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
            <Container sx={{ paddingTop: 4 }}>
              <Breadcrumbs>
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
            </Container>
          </Box>
        </Box>
      </main>
    </div>
  );
};

export default CandidatePage;
