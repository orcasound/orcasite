import { Container, Stack, Typography } from '@mui/material'
import Head from 'next/head'

import FeedCard from '../components/FeedCard'
import { getMapLayout } from '../components/MapLayout'
import { FeedsQuery } from '../generated/types'
import API from '../graphql/apiClient'

export default function HomePage({ feeds }: { feeds: FeedsQuery['feeds'] }) {
  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Typography variant="h4" mt={4}>
            Listen live
          </Typography>
          <Typography variant="body1">
            Select a location to start listening live
          </Typography>
          <Stack spacing={4} mt={4}>
            {feeds.map((feed) => (
              <FeedCard key={feed.id} feed={feed} />
            ))}
          </Stack>
        </Container>
      </main>
    </div>
  )
}

HomePage.getLayout = getMapLayout

export async function getStaticProps() {
  const response = await API.feeds()
  return { props: { feeds: response.feeds } }
}
