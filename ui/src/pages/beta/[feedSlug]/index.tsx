import { useRouter } from "next/router";
import { useEffect } from "react";

import HydrophoneDetailTabs from "@/components/CandidateList/HydrophoneDetailTabs";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
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
