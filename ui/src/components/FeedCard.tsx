import { Card, CardActionArea, CardHeader } from "@mui/material";

import Link from "@/components/Link";
import { Feed } from "@/graphql/generated";
import { useListenerCount } from "@/hooks/useFeedPresence";

export default function FeedCard({
  feed,
}: {
  feed: Pick<Feed, "name" | "slug">;
}) {
  const listenerCount = useListenerCount(feed.slug);
  console.log("listener count", feed.slug, listenerCount);
  return (
    <Link href={`/listen/${feed.slug}`} underline="none">
      <Card variant="outlined">
        <CardActionArea>
          <CardHeader
            title={feed.name}
            // subheader={
            //   `${listenerCount} listening`
            // }
            subheader={
              listenerCount !== undefined && `${listenerCount} listening`
            }
          />
        </CardActionArea>
      </Card>
    </Link>
  );
}
