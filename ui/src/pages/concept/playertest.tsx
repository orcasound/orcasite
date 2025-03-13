import { getModeratorLayout } from "@/components/layouts/ModeratorLayout";
import { DetectionsPlayer } from "@/components/Player/DetectionsPlayer";
import { useCandidateQuery } from "@/graphql/generated";
import { useFeedsQuery } from "@/graphql/generated";
import { CombinedData } from "@/types/DataTypes";

const ModeratorReportsPlayer = ({
  firstDetection,
  lastDetection,
}: {
  firstDetection?: CombinedData;
  lastDetection?: CombinedData;
}) => {
  // const router = useRouter();
  //  const { candidateId } = router.query;
  const candidateId = "cand_02z0tyRhTNcgmch7FIHrXJ";

  const candidateQuery = useCandidateQuery({
    id: (candidateId || "") as string,
  });
  const candidate = candidateQuery.data?.candidate;
  const detections = candidate && candidate.detections;

  // get hydrophone feed list
  const feedsQueryResult = useFeedsQuery();
  const feedsData = feedsQueryResult.data?.feeds ?? [];

  // get feed object from feedId
  // const feedObj = feedsData.find((feed) => feed.id === firstDetection.feedId);
  // const firstTimestamp = firstDetection.playlistTimestamp;
  // const lastTimestamp = lastDetection.playlistTimestamp;

  const offsetPadding = 15;
  const minOffset = detections
    ? Math.min(...detections.map((d) => +d.playerOffset))
    : 0;
  const maxOffset = detections
    ? Math.max(...detections.map((d) => +d.playerOffset))
    : 0;
  const startOffset = Math.max(0, minOffset - offsetPadding);
  const endOffset = maxOffset + offsetPadding;

  return (
    <>
      {candidate && (
        <DetectionsPlayer
          feed={candidate.feed}
          timestamp={candidate.detections[0].playlistTimestamp}
          startOffset={startOffset}
          endOffset={endOffset}
          marks={[]}
        />
      )}
    </>
  );
};

ModeratorReportsPlayer.getLayout = getModeratorLayout;

export default ModeratorReportsPlayer;
