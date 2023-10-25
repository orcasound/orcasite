import { NavigateNext } from "@mui/icons-material";
import { Box, Breadcrumbs, Container, Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";

import Link from "@/components/Link";
import { getMapLayout } from "@/components/MapLayout";
import type { NextPageWithLayout } from "@/pages/_app";

const FeedPage: NextPageWithLayout = () => {
  const router = useRouter();
  const slug = router.query.feed as string;

  if (!slug) return null;

  return (
    <div>
      <Head>
        <title>{`Orcasound - ${slug}`}</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Box>
            <Breadcrumbs
              separator={<NavigateNext />}
              aria-label="breadcrumb"
              sx={{ mt: 2 }}
            >
              <Link href={"/"} color="inherit">
                Home
              </Link>
              <Link href={"/listen"} color="inherit">
                Listen
              </Link>
              <Typography color="textPrimary">dynamic</Typography>
              <Typography color="textPrimary">{slug}</Typography>
            </Breadcrumbs>
            <h1>{slug}</h1>
          </Box>
        </Container>
      </main>
    </div>
  );
};

FeedPage.getLayout = getMapLayout;

export default FeedPage;
