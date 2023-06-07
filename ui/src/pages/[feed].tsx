import { NavigateNext } from '@mui/icons-material'
import { Box, Breadcrumbs, Container, Typography } from '@mui/material'
import Head from 'next/head'
import Image from 'next/legacy/image'
import { useRouter } from 'next/router'

import apiClient from '@/graphql/apiClient'
import { useFeedQuery } from '@/graphql/generated'

import Link from '../components/Link'
import { getMapLayout } from '../components/MapLayout'
import type { NextPageWithLayout } from './_app'

const FeedPage: NextPageWithLayout = () => {
  const router = useRouter()
  const slug = router.query.feed as string
  const feed = useFeedQuery(apiClient, { slug: slug }).data?.feed

  if (!feed) return null

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

export default FeedPage
