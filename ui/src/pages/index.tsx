import { GraphicEq, PlayLessonOutlined } from "@mui/icons-material";
import { Box, Button, Container, Typography } from "@mui/material";
import Head from "next/head";

import { getMapLayout } from "@/components/layouts/MapLayout";
import Link from "@/components/Link";
import type { NextPageWithLayout } from "@/pages/_app";
import { analytics } from "@/utils/analytics";

const HomePage: NextPageWithLayout = () => {
  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Typography variant="h4" mt={4}>
            Listen for Whales!
          </Typography>
          <Typography variant="body1" my={2}>
            Learn what orcas sound like. Then listen live for them on underwater
            microphones (hydrophones).
          </Typography>
          <Box
            sx={{
              display: "flex",
              flexDirection: { xs: "column", md: "row" },
              alignItems: "center",
              justifyContent: "space-evenly",
              my: 6,
              gap: 4,
            }}
          >
            <Link href={"/learn"} underline="none" width={0.9}>
              <Button
                variant="outlined"
                color="secondary"
                size="large"
                startIcon={<PlayLessonOutlined />}
                fullWidth
                sx={{ py: 2 }}
                onClick={() => analytics.about.learnTabClicked()}
              >
                Learn the sounds
              </Button>
            </Link>
            <Link href={"/listen"} underline="none" width={0.9}>
              <Button
                variant="contained"
                color="primary"
                size="large"
                startIcon={<GraphicEq />}
                fullWidth
                sx={{ py: 2 }}
                onClick={() => analytics.about.listenTabClicked()}
              >
                Select listening location
              </Button>
            </Link>
          </Box>

          <Typography variant="body1" my={2}>
            Let us know when you hear them, or any sound you think is
            interesting! That will help researchers and stewards protect the
            orcas and study their acoustic environment.
          </Typography>
          <Typography variant="body1" my={2}>
            You can also get notified when our listeners or AI detect whales at
            any of our hydrophone locations.
          </Typography>
        </Container>
      </main>
    </div>
  );
};

HomePage.getLayout = getMapLayout;

export default HomePage;
