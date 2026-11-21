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
import { useEffect } from "react";

import DetectionsTable from "@/components/DetectionsTable";
import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import SharePreviewTags from "@/components/SharePreviewTags";
import { useCandidateQuery, useGetCurrentUserQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";
import { analytics } from "@/utils/analytics";
import {
  type SharePreview,
  siteOrigin,
  sitePreview,
} from "@/utils/sharePreview";
import { loadCandidatePreview, previewOrNull } from "@/utils/sharePreviewData";

type Props = { preview: SharePreview | null; origin: string };

const CandidatePage: NextPageWithLayout<Props> = ({ preview, origin }) => {
  const router = useRouter();
  const { candidateId } = router.query;

  const candidateQuery = useCandidateQuery({
    id: (candidateId || "") as string,
  });
  const candidate = candidateQuery.data?.candidate;
  const { currentUser } = useGetCurrentUserQuery().data ?? {};

  // In an effect so it runs once per report in the browser, not on every
  // render, and not on the server, which now renders this page too.
  useEffect(() => {
    if (candidateId && typeof candidateId === "string") {
      analytics.reports.reportOpened(candidateId);
    }
  }, [candidateId]);

  const time = candidate && new Date(candidate.minTime).toISOString();
  const categoryMap = {
    WHALE: "biophony",
    VESSEL: "anthrophony",
    OTHER: "geophony",
  };
  const category = candidate?.category && categoryMap[candidate.category];

  return (
    <div>
      <Head>
        <title>{`${preview?.title ?? `Report ${candidateId}`} | Orcasound`}</title>
      </Head>
      <SharePreviewTags
        preview={
          preview ?? {
            ...sitePreview(),
            path: `/reports/${encodeURIComponent(String(candidateId))}`,
          }
        }
        origin={origin}
      />

      <main>
        <Breadcrumbs sx={{ paddingTop: 4 }}>
          <Link underline="hover" color="inherit" href={"/reports"}>
            Reports
          </Link>
          <Typography>{candidate?.id}</Typography>
        </Breadcrumbs>

        <Paper sx={{ marginTop: 4, overflow: "auto" }}>
          <Box
            sx={{
              p: 5,
            }}
          >
            <Box
              sx={{
                display: "flex",
                justifyContent: "space-between",
                mb: 5,
              }}
            >
              <Box
                sx={{
                  display: "flex",
                  flexDirection: "column",
                  gap: 1,
                }}
              >
                <Typography variant="h4">Detections</Typography>
                <Box
                  sx={{
                    display: "flex",
                    gap: 1,
                    alignItems: "center",
                  }}
                >
                  {currentUser?.moderator && (
                    <Chip
                      variant="outlined"
                      label={candidate?.visible ? "Visible" : "Hidden"}
                    />
                  )}
                  <Typography variant="body2">{candidate?.id}</Typography>
                </Box>
              </Box>
              <Box
                sx={{
                  display: "flex",
                  gap: 1,
                }}
              >
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

// Rendered on the server only for the link preview. The page still loads the
// report in the browser, with the visitor's sign-in, so moderators see hidden
// detections; only the preview's few fields come from here.
export function getStaticPaths() {
  return { paths: [], fallback: "blocking" };
}

export async function getStaticProps({
  params,
}: {
  params: { candidateId: string };
}) {
  // A missing or hidden report gets the site's preview. It is not a 404:
  // moderators open hidden reports here.
  const preview = await previewOrNull(
    () => loadCandidatePreview(params.candidateId),
    "candidate",
    params.candidateId,
  );

  return {
    props: { preview, origin: siteOrigin() } satisfies Props,
    // Moderators can hide a report or change its category.
    revalidate: 60,
  };
}

export default CandidatePage;
