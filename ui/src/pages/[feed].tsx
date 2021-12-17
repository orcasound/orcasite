import { Box, Container } from '@mui/material'
import Head from 'next/head'
import Image from 'next/image'

import { getMapLayout } from '../components/MapLayout'
import { Feed } from '../generated/types'
import API from '../graphql/apiClient'

export default function FeedPage({ feed }: { feed: Feed }) {
  return (
    <div>
      <Head>
        <title>Orcasound - {feed.name}</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Box>
            <h1>{feed.name}</h1>
            <div
              style={{ position: 'relative', width: '100%', height: '15em' }}
            >
              <Image
                src={feed.thumbUrl}
                layout="fill"
                alt=""
                objectFit="contain"
                objectPosition="left"
              />
            </div>
            <div dangerouslySetInnerHTML={{ __html: feed.introHtml }} />
            <div
              style={{ position: 'relative', width: '100%', height: '15em' }}
            >
              <Image
                src={feed.mapUrl}
                layout="fill"
                alt=""
                objectFit="contain"
                objectPosition="left"
              />
            </div>
          </Box>
        </Container>
      </main>
    </div>
  )
}

FeedPage.getLayout = getMapLayout

export async function getStaticPaths() {
  const response = await API.feeds()
  return {
    paths: response.feeds.map((feed) => ({ params: { feed: feed.slug } })),
    fallback: 'blocking',
  }
}

export async function getStaticProps({ params }: { params: { feed: string } }) {
  const response = await API.feed({ slug: params.feed })
  if (!response.feed) return { notFound: true }
  return { props: { feed: response.feed } }
}
