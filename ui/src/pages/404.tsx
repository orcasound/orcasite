import { Box, Button, Container, Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";
import { useEffect, useState } from "react";

import { getSimpleLayout } from "@/components/layouts/SimpleLayout";
import Link from "@/components/Link";
import type { NextPageWithLayout } from "@/pages/_app";

const Custom404: NextPageWithLayout = () => {
  const router = useRouter();
  const [countdown, setCountdown] = useState(10);

  useEffect(() => {
    const timer = setInterval(() => {
      setCountdown((prev) => {
        if (prev <= 1) {
          router.push("/");
          return 0;
        }
        return prev - 1;
      });
    }, 1000);

    return () => clearInterval(timer);
  }, [router]);

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

            <Typography variant="body1" color="text.secondary" sx={{ mb: 2 }}>
              The page you&#39;re looking for doesn&#39;t exist.
            </Typography>

            <Typography variant="body2" color="text.secondary">
              Redirecting to home in <strong>{countdown}</strong> second
              {countdown !== 1 ? "s" : ""}...
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
