import { Box, Container, Typography } from "@mui/material";
import Head from "next/head";

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

const HomePage: NextPageWithLayout = () => {
  const feeds = useFeedsQuery().data?.feeds;

  if (!feeds) return null;

  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Typography variant="h5" mt={4}>
            Listen for Whales!
          </Typography>
          <Typography variant="body1" my={2}>
            Learn what orcas sound like. Then listen live for them on underwater
            microphones (hydrophones).
          </Typography>
          <Typography variant="body1" my={2}>
            Let us know when you hear them, or any sound you think is
            interesting! That will help researchers and stewards protect the
            orcas and their environment.
          </Typography>
          <Typography variant="body1" my={2}>
            You can also get notified when our listeners or algorithms detect
            whales at any of our hydrophone locations.
          </Typography>
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

HomePage.getLayout = getMapLayout;

export default HomePage;
