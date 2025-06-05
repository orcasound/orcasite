import { ArrowBackIos } from "@mui/icons-material";
import { PlayCircle } from "@mui/icons-material";
import {
  Box,
  Container,
  Stack,
  Theme,
  Typography,
  useMediaQuery,
  useTheme,
} from "@mui/material";
import Head from "next/head";
import Link from "next/link";
import { useRouter } from "next/router";
import { ReactNode } from "react";

import { timeRangeSelect } from "@/components/CandidateList/CandidateListFilters";
import { useData } from "@/context/DataContext";
import darkTheme from "@/styles/darkTheme";

const HydrophoneDetailTabs = ({ children }: { children: ReactNode }) => {
  const router = useRouter();
  const { feedSlug } = router.query;
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const theme = useTheme();
  const { feeds, filters } = useData();
  const feed = feeds.find((feed) => feed.slug === feedSlug);

  // const isCandidateDetail =
  //   !!router.query.feedSlug && !!router.query.candidateId;

  const href =
    // isCandidateDetail
    //   ? `/beta/candidates/${feed?.slug}/${router.query.candidateId}`
    //   :
    `/beta`;

  const route = router.route.split("/");
  const tabPage = route[route.length - 1];
  const isIndexPage = route[route.length - 1] === "[feedSlug]";
  const isCandidatePage = route[route.length - 1] === "[candidateId]";

  type Tab = {
    title: string;
    slug: string;
  };

  const tabs = [
    { title: "About", slug: "" },
    {
      title:
        timeRangeSelect.find((el) => el.value === filters.timeRange)?.label ??
        "Candidates",
      slug: "candidates",
    },
    { title: "Greatest Hits", slug: "#" },
  ];

  const tabRow = (tabs: Tab[]) => (
    <Stack
      direction="row"
      gap="40px"
      sx={{
        marginBottom: "24px",
        borderBottom: "1px solid rgba(255,255,255,.33)",
        px: 3,
        justifyContent: "center",
      }}
    >
      {tabs.map((tab) => {
        const tabSlug = tab.slug;
        const active = isIndexPage
          ? tabSlug === ""
          : isCandidatePage
            ? tabSlug === "candidates"
            : tabPage === tabSlug;
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
      <Container sx={{ px: 0 }}>
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
            p: 2,
            minHeight: smDown ? " 160px" : "260px",
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
            href={href}
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
        <Box sx={{ p: 2 }}>{children}</Box>
      </Container>
    </div>
  );
};

export default HydrophoneDetailTabs;
