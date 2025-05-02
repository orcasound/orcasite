import { Stack } from "@mui/material";

import CandidateCard from "@/components/CandidateList/CandidateCard";
import { useData } from "@/context/DataContext";
import { Candidate } from "@/types/DataTypes";

export default function CandidatesList() {
  const { sortedCandidates } = useData();

  const candidateCards = sortedCandidates.map((candidate: Candidate) => (
    <CandidateCard candidate={candidate} key={candidate.id} />
  ));

  return <Stack spacing={2}>{candidateCards}</Stack>;
}
