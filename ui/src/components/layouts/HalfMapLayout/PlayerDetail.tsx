import { KeyboardArrowDown } from "@mui/icons-material";
import Box from "@mui/material/Box";
import { Dispatch, SetStateAction } from "react";

import { useData } from "@/context/DataContext";
import { useNowPlaying } from "@/context/NowPlayingContext";

// placeholder -- after merge, copy BoutPage to this compoonent

export default function PlayerDetail({
  setPlaybarExpanded,
}: {
  setPlaybarExpanded: Dispatch<SetStateAction<boolean>>;
}) {
  const { nowPlayingCandidate } = useNowPlaying();
  const { feeds } = useData();
  const feed =
    feeds.find((f) => f.id === nowPlayingCandidate?.feedId) ?? feeds[0];

  return (
    <>
      <Box
        className="drawer-nav"
        sx={{
          display: "flex",
          position: "fixed",
          left: 0,
          ml: "2rem",
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
      {/* {nowPlayingCandidate && <AudioAnalyzer isNew={true} feed={feed} />} */}
    </>
  );
}
