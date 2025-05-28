import { ArrowBackIos } from "@mui/icons-material";
import { Box, Container, Stack, Typography, useTheme } from "@mui/material";
import Head from "next/head";
import Link from "next/link";
import { useRouter } from "next/router";
import { ReactNode } from "react";

import { useData } from "@/context/DataContext";
import darkTheme from "@/styles/darkTheme";

const HydrophoneDetailTabs = ({ children }: { children: ReactNode }) => {
  const router = useRouter();
  const { feedSlug } = router.query;
  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const theme = useTheme();
  const { feeds } = useData();
  const feed = feeds.find((feed) => feed.slug === feedSlug);

  const routerPath = router.asPath.split("/");
  const feedPage = routerPath[routerPath.length - 1];

  const tabs = ["Candidates", "About", "Take Action"];

  const tabRow = (tabs: string[]) => (
    <Stack
      direction="row"
      gap="40px"
      sx={{
        marginBottom: "24px",
        borderBottom: "1px solid rgba(255,255,255,.33)",
        px: { xs: 1, sm: 2, md: 3 },
      }}
    >
      {tabs.map((tab) => {
        const active = tab.toLowerCase() === feedPage;
        const tabRoute = tab.split(" ").join("-").toLowerCase();
        return (
          <Link
            key={tab}
            href={`/beta/${feedSlug}/${tabRoute}`}
            style={{
              color: active
                ? darkTheme.palette.text.primary
                : darkTheme.palette.text.secondary,
              textDecoration: "none",
              height: "100%",
              padding: "16px 0",
              borderBottom: active
                ? "1px solid " + darkTheme.palette.accent3.main
                : "none",
            }}
          >
            {tab}
          </Link>
        );
      })}
    </Stack>
  );
  return (
    <div>
      <Head>Report {feedSlug} | Orcasound </Head>
      <Container
        maxWidth="xl"
        sx={{
          px: { xs: 1, sm: 2, md: 3 },
        }}
      >
        <Box
          sx={{
            marginTop: 4,
            display: "flex",
            alignItems: "flex-start",
            flexDirection: "column",
            justifyContent: "space-between",
            gap: "16px",
            marginBottom: "8px",
          }}
        >
          <Link
            href={`/beta/${feed?.slug}`}
            style={{
              display: "flex",
              alignItems: "center",
              gap: "8px",
              textDecoration: "none",
              lineHeight: 1,
              color: theme.palette.common.white,
            }}
          >
            <ArrowBackIos />
          </Link>
          <Typography variant="h4" component="h1" sx={{ flex: 1 }}>
            {feed?.name}
          </Typography>
        </Box>
        <Box
          sx={{
            mx: { xs: -1, sm: -2, md: -3 },
          }}
        >
          {tabRow(tabs)}
        </Box>
        {children}
      </Container>
    </div>
  );
};

export default HydrophoneDetailTabs;
