import { Container, Stack, Typography } from '@mui/material'
import Head from 'next/head'

import { useFeedsQuery } from '@/graphql/generated'

import FeedCard from '../components/FeedCard'
import { getMapLayout } from '../components/MapLayout'
import type { NextPageWithLayout } from './_app'

const HomePage: NextPageWithLayout = () => {
  const feeds = useFeedsQuery().data?.feeds

  if (!feeds) return null

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

export default HomePage
