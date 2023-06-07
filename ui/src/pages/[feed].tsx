import { NavigateNext } from '@mui/icons-material'
import { Box, Breadcrumbs, Container, Typography } from '@mui/material'
import Head from 'next/head'
import Image from 'next/legacy/image'

import Link from '../components/Link'
import { getMapLayout, getMapProps } from '../components/MapLayout'
import { Feed } from '../generated/types'
import API from '../graphql/apiClient'
import type { NextPageWithLayout } from './_app'

type Props = { feed: Feed }

const FeedPage: NextPageWithLayout<Props> = ({ feed }) => {
  return (
    <div>
      <Head>
        <title>Orcasound - {feed.name}</title>
      </Head>

      <main>
        <Container maxWidth="sm">
          <Box>
            <Breadcrumbs separator={<NavigateNext />} aria-label="breadcrumb">
              <Link href={'/'} color="inherit">
                All hydrophones
              </Link>
              <Typography color="textPrimary">{feed.name}</Typography>
            </Breadcrumbs>
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
  const mapProps = await getMapProps()
  return { props: { ...mapProps, feed: response.feed } }
}

export default FeedPage
