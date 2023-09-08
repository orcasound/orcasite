import { Box, Container } from "@mui/material";
import Head from "next/head";

import Header from "@/components/Header";
import type { NextPageWithLayout } from "@/pages/_app";

const DetectionsPage: NextPageWithLayout = () => {
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
            </Container>
          </Box>
        </Box>
      </main>
    </div>
  );
};

export default DetectionsPage;
