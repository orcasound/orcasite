import type { Theme } from "@mui/material";
import { Stack } from "@mui/material";
import { useMediaQuery } from "@mui/material";

import CandidateCard from "@/components/CandidateList/CandidateCard";
import { useData } from "@/context/DataContext";
import { useLayout } from "@/context/LayoutContext";
import { Candidate } from "@/types/DataTypes";

export default function CandidatesList() {
  // layout
  const layout = useLayout();
  const lgUp = useMediaQuery((theme: Theme) => theme.breakpoints.up("lg"));

  // data
  const { sortedCandidates } = useData();

  // card list
  const candidateCards = sortedCandidates.map((candidate: Candidate) => (
    <CandidateCard
      candidate={candidate}
      key={
        candidate.array[0].timestampString +
        "-" +
        candidate.array[candidate.array.length - 1].timestampString
      }
    />
  ));

  return <Stack spacing={2}>{candidateCards}</Stack>;
}
