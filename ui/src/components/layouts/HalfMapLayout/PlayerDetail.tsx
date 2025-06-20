import { KeyboardArrowDown } from "@mui/icons-material";
import { Theme, useMediaQuery } from "@mui/material";
import Box from "@mui/material/Box";
import { useMemo } from "react";

import AudioAnalyzer from "@/components/Bouts/beta/AudioAnalyzer";
import { CandidatesStack } from "@/components/CandidateList/CandidatesStack";
import HydrophoneDetailTabs from "@/components/CandidateList/HydrophoneDetailTabs";
import { useData } from "@/context/DataContext";
import { useLayout } from "@/context/LayoutContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { useBout, useBouts } from "@/hooks/beta/useBouts";

export default function PlayerDetail() {
  const { setPlaybarExpanded, candidatePreview, setCandidatePreview } =
    useLayout();
  const { nowPlayingFeed, nowPlayingCandidate } = useNowPlaying();

  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  // const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  // seeding with known bout from production
  const { feeds } = useData();
  const portTownsend =
    feeds.find((f) => f.name === "Port Townsend") ?? feeds[0];
  const feed = useMemo(() => {
    if (nowPlayingCandidate) {
      return feeds.find((f) => f.id === nowPlayingCandidate.feedId);
    } else if (nowPlayingFeed) {
      return nowPlayingFeed;
    } else {
      return undefined;
    }
  }, [nowPlayingCandidate, nowPlayingFeed, feeds]);

  const { data } = useBouts({ feedId: portTownsend?.id });
  const bouts = data?.bouts?.results ?? [];
  const boutQueryResult = useBout({
    id: bouts[0]?.id || "",
    enabled: !!bouts[0]?.id,
  });
  const bout = boutQueryResult.data?.bout;

  return (
    <>
      <Box
        className="drawer-nav"
        sx={{
          display: "flex",
          position: "fixed",
          left: 0,
          ml: mdDown ? "1rem" : "2rem",
          mt: "1.25rem",
          borderRadius: "100%",
          padding: ".25rem",
          backgroundColor: "rgba(255,255,255,.15)",
          zIndex: 1000,
          "&:hover": {
            backgroundColor: "rgba(255,255,255,.25)",
          },
        }}
      >
        <KeyboardArrowDown
          sx={{ fontSize: "2rem" }}
          onClick={() => {
            setPlaybarExpanded(false);
            setCandidatePreview(true); // in case it was set to false by 'open audio analyzer' button
          }}
        />
      </Box>

      {nowPlayingCandidate && !candidatePreview && (
        <AudioAnalyzer isNew={false} bout={bout} feed={portTownsend} />
      )}
      {(nowPlayingFeed || (nowPlayingCandidate && candidatePreview)) && (
        <>
          <HydrophoneDetailTabs drawer={true} feed={feed}>
            <CandidatesStack feed={feed} showChart={true} />
          </HydrophoneDetailTabs>
        </>
      )}
    </>
  );
}
