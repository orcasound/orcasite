import { Box, Button, Container, Typography } from "@mui/material";
import Head from "next/head";

import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import Link from "@/components/Link";
import type { NextPageWithLayout } from "@/pages/_app";

const Custom404: NextPageWithLayout = () => {
  return (
    <div>
      <Head>
        <title>Page Not Found | Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Box
            sx={{
              display: "flex",
              flexDirection: "column",
              alignItems: "center",
              justifyContent: "center",
              textAlign: "center",
              minHeight: "50vh",
              mt: 4,
              gap: 3,
            }}
          >
            <Typography
              variant="h1"
              color="primary"
              sx={{ fontSize: "6rem", fontWeight: "bold" }}
            >
              404
            </Typography>

            <Typography variant="h4" gutterBottom>
              Page Not Found
            </Typography>

            <Typography
              variant="body1"
              color="text.secondary"
              sx={{ mb: 2, textWrap: "balance" }}
            >
              {"The page you're looking for doesn't exist."}
            </Typography>

            <Link href="/" underline="none">
              <Button
                variant="contained"
                color="primary"
                size="large"
                sx={{ mt: 2 }}
              >
                Go Home Now
              </Button>
            </Link>
          </Box>
        </Container>
      </main>
    </div>
  );
};

Custom404.getLayout = getSimpleLayout;

export default Custom404;
