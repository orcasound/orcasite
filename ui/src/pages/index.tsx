import { Box, Container } from '@mui/material'
import Head from 'next/head'

import Link from '../components/Link'
import { Feed } from '../generated/types'
import API from '../graphql/apiClient'

export default function HomePage({ feeds }: { feeds: Feed[] }) {
  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          {feeds.map((feed) => (
            <Box key={feed.id}>
              <Link
                href={{
                  pathname: '/[slug]',
                  query: { slug: feed.slug },
                }}
              >
                {feed.name}
              </Link>
            </Box>
          ))}
        </Container>
      </main>
    </div>
  )
}

export async function getStaticProps() {
  const response = await API.feeds()
  return { props: { feeds: response.feeds } }
}
