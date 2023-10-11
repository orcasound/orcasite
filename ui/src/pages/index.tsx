import { Box, Button, Container, Typography } from "@mui/material";
import Head from "next/head";

import Link from "@/components/Link";
import { getMapLayout } from "@/components/MapLayout";
import type { NextPageWithLayout } from "@/pages/_app";

const HomePage: NextPageWithLayout = () => {
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

          <Box
            display="flex"
            flexDirection="column"
            justifyContent="space-evenly"
            alignItems="center"
            mt={5}
            gap={3}
          >
            {/* TODO: Figure out how/where to show learning section */}
            {/* <Link href={"/learn"} underline="none">
              <Button variant="outlined" color="secondary" size="large">
                Learn the sounds
              </Button>
            </Link> */}
            <Link href={"/feeds"} underline="none">
              <Button variant="contained" color="primary" size="large">
                Start listening
              </Button>
            </Link>
          </Box>
        </Container>
      </main>
    </div>
  );
};

HomePage.getLayout = getMapLayout;

export default HomePage;
