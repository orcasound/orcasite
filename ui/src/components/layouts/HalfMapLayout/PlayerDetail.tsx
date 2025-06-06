import { KeyboardArrowDown } from "@mui/icons-material";
import Box from "@mui/material/Box";
import { Dispatch, SetStateAction } from "react";

import BoutPage from "@/components/Bouts/BoutPage";
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
        display="flex"
        className="drawer-nav"
        justifyContent="flex-end"
        alignItems="center"
        my={2}
      >
        <KeyboardArrowDown
          sx={{ fontSize: "2rem" }}
          onClick={() => setPlaybarExpanded(false)}
        />
      </Box>
      {nowPlayingCandidate && <BoutPage isNew={true} feed={feed} />}
    </>
  );
}
