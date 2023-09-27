import { ArrowBack, NavigateNext } from "@mui/icons-material";
import {
  Box,
  Breadcrumbs,
  Container,
  IconButton,
  Typography,
} from "@mui/material";
import Head from "next/head";

import Link from "@/components/Link";
import { getMapLayout } from "@/components/MapLayout";
import { useFeedsQuery } from "@/graphql/generated";
import type { NextPageWithLayout } from "@/pages/_app";

const examples = [
  {
    title: "Calls",
    audio:
      "https://www.orcasound.net/data/product/SRKW/orcasite/call-examples.mp3",
  },
  {
    title: "Clicks",
    audio:
      "https://orcasound.net/data/product/SRKW/clicks/20190705-JK_varied_clicks-10sec.mp3",
  },
  {
    title: "Whistles",
    audio:
      "https://www.orcasound.net/data/product/SRKW/orcasite/whistle-examples.mp3",
  },
];

const LearnPage: NextPageWithLayout = () => {
  const feeds = useFeedsQuery().data?.feeds;

  if (!feeds) return null;

  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Box display="flex" alignItems="center" sx={{ ml: -2, mt: 2 }}>
            <Link href={"/"} underline="none" color="inherit">
              <IconButton>
                <ArrowBack />
              </IconButton>
            </Link>
            <Breadcrumbs separator={<NavigateNext />} aria-label="breadcrumb">
              <Link href={"/"} color="inherit">
                Home
              </Link>
              <Typography color="textPrimary">Learn</Typography>
            </Breadcrumbs>
          </Box>
          <Typography variant="h5" mt={4}>
            What do orcas sound like?
          </Typography>
          <Typography variant="body1" my={2}>
            Here are some samples of calls, clicks, and whistles that are made
            by southern resident killer whales:
          </Typography>
          <Box>
            {examples.map((example) => (
              <Box key={example.title} my={2}>
                <Typography variant="h6">{example.title}</Typography>
                <Box mt={1}>
                  <audio
                    controls
                    src={example.audio}
                    // TODO: Re-add analytics
                    // onPlay={() =>
                    //   analyticsEvents.about.sampleAudioPlayed(example.title)
                    // }
                  />
                </Box>
              </Box>
            ))}
          </Box>
        </Container>
      </main>
    </div>
  );
};

LearnPage.getLayout = getMapLayout;

export default LearnPage;
