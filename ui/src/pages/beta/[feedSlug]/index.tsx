import { Paper, Typography } from "@mui/material";
import { useRouter } from "next/router";
import { useEffect } from "react";

import HydrophoneDetailTabs from "@/components/CandidateList/HydrophoneDetailTabs";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import type { NextPageWithLayout } from "@/pages/_app";

const HydrophonePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { feedSlug } = router.query;
  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));

  const { setNowPlayingFeed, setNowPlayingCandidate } = useNowPlaying();
  const { feeds } = useData();
  const feed = feeds.find((feed) => feed.slug === feedSlug);

  // load the feed into nowPlaying on page load
  useEffect(() => {
    if (feed) {
      setNowPlayingFeed(feed);
      setNowPlayingCandidate(null);
    }
  }, [feed, setNowPlayingCandidate, setNowPlayingFeed]);

  return (
    <HydrophoneDetailTabs>
      {/* <div
        className="image"
        style={{ position: "relative", width: "100%", height: "15em" }}
      >
        {feed?.imageUrl && (
          <Image
            src={feed?.imageUrl}
            layout="fill"
            alt=""
            objectFit="contain"
            objectPosition="left"
          />
        )}{" "}
      </div> */}
      <Paper
        elevation={0}
        sx={{
          backgroundColor: "accent1.main",
          p: 2,
          borderRadius: 1,
          mt: 2,
        }}
      >
        <Typography variant="body2">
          Hosted by <strong>Beam Reach</strong>
          <br />
          <Link href="#" target="_blank" rel="noopener">
            Learn more or donate
          </Link>{" "}
          to support their work.
        </Typography>
      </Paper>

      {feed?.introHtml ? (
        <div
          className="intro"
          dangerouslySetInnerHTML={{ __html: feed?.introHtml }}
        />
      ) : (
        JSON.stringify(feed, null, 2)
      )}
    </HydrophoneDetailTabs>
  );
};

HydrophonePage.getLayout = getHalfMapLayout;

export default HydrophonePage;
