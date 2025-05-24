import { Stack } from "@mui/material";

import CandidateCard from "@/components/CandidateList/CandidateCard";
import { useData } from "@/context/DataContext";
import { Feed } from "@/graphql/generated";
import { Candidate } from "@/types/DataTypes";

export default function CandidatesList({ feed }: { feed?: Feed }) {
  const { sortedCandidates } = useData();

  const candidates = feed
    ? sortedCandidates.filter((c) => c.feedId === feed.id)
    : sortedCandidates;

  const candidateCards = candidates.map((candidate: Candidate) => (
    <CandidateCard candidate={candidate} key={candidate.id} />
  ));

  return <Stack spacing={2}>{candidateCards}</Stack>;
}
