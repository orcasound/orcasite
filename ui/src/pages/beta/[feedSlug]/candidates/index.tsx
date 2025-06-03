import { useRouter } from "next/router";
import { useEffect } from "react";

import { CandidatesStack } from "@/components/CandidateList/CandidatesStack";
import HydrophoneDetailTabs from "@/components/CandidateList/HydrophoneDetailTabs";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import type { NextPageWithLayout } from "@/pages/_app";

const HydrophoneCandidatesPage: NextPageWithLayout = () => {
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
      <CandidatesStack feed={feed} showChart={true} />
    </HydrophoneDetailTabs>
  );
};

HydrophoneCandidatesPage.getLayout = getHalfMapLayout;

export default HydrophoneCandidatesPage;
