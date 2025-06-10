import { KeyboardArrowDown } from "@mui/icons-material";
import { Theme, useMediaQuery } from "@mui/material";
import Box from "@mui/material/Box";

import { useData } from "@/context/DataContext";
import { useLayout } from "@/context/LayoutContext";
import { useNowPlaying } from "@/context/NowPlayingContext";
import { useBout, useBouts } from "@/hooks/beta/useBouts";

import AudioAnalyzer from "../../Bouts/beta/AudioAnalyzer";

// placeholder -- after merge, copy BoutPage to this compoonent

export default function PlayerDetail() {
  const { nowPlayingCandidate } = useNowPlaying();
  const { setPlaybarExpanded } = useLayout();

  const mdDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("md"));
  const smDown = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));

  // seeding with known bout from production
  const { feeds } = useData();
  const feed = feeds.find((f) => f.name === "Port Townsend") ?? feeds[0];
  const { data } = useBouts({ feedId: feed?.id });
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
          "&:hover": {
            backgroundColor: "rgba(255,255,255,.25)",
          },
        }}
      >
        <KeyboardArrowDown
          sx={{ fontSize: "2rem" }}
          onClick={() => setPlaybarExpanded(false)}
        />
      </Box>

      {nowPlayingCandidate && (
        <AudioAnalyzer isNew={false} bout={bout} feed={feed} />
      )}
    </>
  );
}
