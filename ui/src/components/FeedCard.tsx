import { Card, CardActionArea, CardHeader } from '@mui/material'

import { Feed } from '@/graphql/generated'

import Link from '../components/Link'

export default function FeedCard({
  feed,
}: {
  feed: Pick<Feed, 'name' | 'slug'>
}) {
  return (
    <Link href={`/${feed.slug}`} underline="none">
      <Card variant="outlined">
        <CardActionArea>
          <CardHeader title={feed.name} />
        </CardActionArea>
      </Card>
    </Link>
  )
}
