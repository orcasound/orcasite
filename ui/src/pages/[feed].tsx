import Head from 'next/head'
import Image from 'next/image'

import { getFeed, listFeeds } from '../queries/feed'

export default function Feed({ feed }: any) {
  return (
    <div>
      <Head>
        <title>Orcasound - {feed.name}</title>
      </Head>

      <main>
        <h1>{feed.name}</h1>
        <div style={{ position: 'relative', width: '100%', height: '15em' }}>
          <Image
            src={feed.thumbUrl}
            layout="fill"
            alt=""
            objectFit="contain"
            objectPosition="left"
          />
        </div>
        <div dangerouslySetInnerHTML={{ __html: feed.introHtml }} />
        <div style={{ position: 'relative', width: '100%', height: '15em' }}>
          <Image
            src={feed.mapUrl}
            layout="fill"
            alt=""
            objectFit="contain"
            objectPosition="left"
          />
        </div>
      </main>
    </div>
  )
}

export async function getStaticPaths() {
  const response = await listFeeds()
  return {
    paths: response.feeds.map((feed: any) => ({ params: { feed: feed.slug } })),
    fallback: 'blocking',
  }
}

export async function getStaticProps({ params }: any) {
  const response = await getFeed(params.feed)

  if (!response.feed) {
    return {
      notFound: true,
    }
  }

  return { props: { feed: response.feed } }
}
