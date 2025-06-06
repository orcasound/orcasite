import { Paper, Typography } from "@mui/material";
import { useRouter } from "next/router";
import { useEffect } from "react";

import HydrophoneDetailTabs from "@/components/CandidateList/HydrophoneDetailTabs";
import { getHalfMapLayout } from "@/components/layouts/HalfMapLayout/HalfMapLayout";
import Link from "@/components/Link";
import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import type { NextPageWithLayout } from "@/pages/_app";

const hosts = [
  {
    hydrophone: "orcasound-lab",
    name: "Beam Reach",
    link: "http://www.beamreach.blue/",
  },
  {
    hydrophone: "north-sjc",
    name: "Orca Behavior Institute",
    link: "https://www.orcabehaviorinstitute.org/",
  },
  {
    hydrophone: "sunset-bay",
    name: "Beach Camp at Sunset Bay",
    link: "https://www.sunsetbaywharf.com/",
  },
  {
    hydrophone: "port-townsend",
    name: "Port Townsend Marine Science Center",
    link: "http://www.ptmsc.org/",
  },
  {
    hydrophone: "bush-point",
    name: "Orca Network",
    link: "https://orcanetwork.org/",
  },
];

const HydrophonePage: NextPageWithLayout = () => {
  const router = useRouter();
  const { feedSlug } = router.query;
  // const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));

  const { setNowPlayingFeed, setNowPlayingCandidate } = useNowPlaying();
  const { feeds, autoPlayOnReady } = useData();
  const feed = feeds.find((feed) => feed.slug === feedSlug);
  const host = hosts.find((host) => feedSlug === host.hydrophone);
  // load the feed into nowPlaying on page load
  useEffect(() => {
    if (feed) {
      setNowPlayingFeed(feed);
      setNowPlayingCandidate(null);
      autoPlayOnReady.current = false;
    }
  }, [feed, setNowPlayingCandidate, setNowPlayingFeed, autoPlayOnReady]);
  console.log("index router.route", router.route);

  return (
    <HydrophoneDetailTabs>
      {host && (
        <Paper
          elevation={0}
          sx={{
            backgroundColor: "accent1.main",
            p: 2,
            borderRadius: 1,
          }}
        >
          <Typography variant="body2">
            Hosted by <strong>{host.name}</strong>
            <br />
            <Link href={host.link} target="_blank" rel="noopener">
              Learn more or donate
            </Link>{" "}
            to support their work.
          </Typography>
        </Paper>
      )}

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
