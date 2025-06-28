// utils/pageContext.ts
import { NextRouter } from "next/router";

export function getPageContext(router: NextRouter) {
  const { route } = router;

  const feedDetailRoutes = [
    "/beta/[feedSlug]/candidates",
    "/beta/[feedSlug]/[candidateId]",
  ];
  const allCandidatesRoutes = [
    "/beta",
    "/beta/candidates/[feedSlug]/[candidateId]",
  ];

  if (feedDetailRoutes.includes(route)) return { isFeedDetail: true };
  if (allCandidatesRoutes.includes(route)) return { isFeedDetail: false };

  return { isFeedDetail: undefined }; // or null, depending on your needs
}
