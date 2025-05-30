import { ArrowBackIos } from "@mui/icons-material";
import { PlayCircle } from "@mui/icons-material";
import { Box, Container, Stack, Typography, useTheme } from "@mui/material";
import { AnimatePresence, motion } from "framer-motion";
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

  const route = router.route.split("/");
  const tabPage = route[route.length - 1];
  const isIndexPage = tabPage === "[feedSlug]";

  type Tab = {
    title: string;
    slug: string;
  };

  const tabs = [
    { title: "About", slug: "" },
    { title: "Candidates", slug: "candidates" },
    { title: "Status", slug: "#" },
  ];

  const tabRow = (tabs: Tab[]) => (
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
        const tabSlug = tab.slug;
        const active = isIndexPage ? tabSlug === "" : tabPage === tabSlug;
        return (
          <Link
            key={tab.title}
            href={`/beta/${feedSlug}/${tabSlug}`}
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
            {tab.title}
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
            position: "relative",
            // marginTop: 5,
            marginBottom: "2px",
            display: "flex",
            flexDirection: "column",
            justifyContent: "space-between",
            gap: "16px",
            background: `center / cover no-repeat url(${feed?.imageUrl})`,
            mx: { xs: -1, sm: -2, md: -3 },
            p: { xs: 1, sm: 2, md: 3 },
            paddingTop: 5,
            minHeight: "260px",
          }}
        >
          {/* Gradient overlay */}
          <Box
            sx={{
              position: "absolute",
              inset: 0,
              background:
                "linear-gradient(to top, rgba(0,0,0,0.33), rgba(0,0,0,0))",
              zIndex: 0,
            }}
          />
          <Link
            href={`/beta`}
            style={{
              display: "flex",
              alignItems: "center",
              gap: "8px",
              textDecoration: "none",
              lineHeight: 1,
              color: theme.palette.common.white,
              zIndex: 1,
              position: "relative",
            }}
          >
            <ArrowBackIos />
          </Link>
          <Box
            sx={{
              width: "100%",
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
            }}
          >
            <Typography
              variant="h4"
              component="h1"
              sx={{
                zIndex: 1,
                lineHeight: 1.1,
                position: "relative",
                textShadow: "0 2px 4px rgba(0, 0, 0, 0.5)",
              }}
            >
              {feed?.name}
            </Typography>
            <PlayCircle
              sx={{ height: 48, width: 48, zIndex: 1, position: "relative" }}
            />
          </Box>
        </Box>
        <Box
          sx={{
            mx: { xs: -1, sm: -2, md: -3 },
          }}
        >
          {tabRow(tabs)}
        </Box>
        <AnimatePresence mode="wait">
          <motion.div
            key={router.asPath}
            initial={{ x: 100, opacity: 0 }}
            animate={{ x: 0, opacity: 1 }}
            exit={{ x: 100, opacity: 0 }}
            transition={{ duration: 0.4, ease: "easeOut" }}
            style={{ height: "100%" }}
          >
            {children}
          </motion.div>
        </AnimatePresence>
      </Container>
    </div>
  );
};

export default HydrophoneDetailTabs;
