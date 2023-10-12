import { Card, CardActionArea, CardHeader } from "@mui/material";

import Link from "@/components/Link";
import { Feed } from "@/graphql/generated";

export default function FeedCard({
  feed,
}: {
  feed: Pick<Feed, "name" | "slug">;
}) {
  return (
    <Link href={`/listen/${feed.slug}`} underline="none">
      <Card variant="outlined">
        <CardActionArea>
          <CardHeader title={feed.name} />
        </CardActionArea>
      </Card>
    </Link>
  );
}
