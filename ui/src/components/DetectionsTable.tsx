import { Candidate, Detection, Feed } from "@/graphql/generated";
import { Box } from "@mui/material";


export default function DetectionsTable({
  detections,
  feed,
  candidate
}: { detections: Detection[], feed: Feed, candidate: Candidate}) {
  return <Box>
    {JSON.stringify(detections)}
  </Box>;
}