import { gql, request } from 'graphql-request'
import Head from 'next/head'
import Link from 'next/link'

export default function Home({ feeds }: { feeds: any }) {
  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        {feeds.map((feed: any) => (
          <div key={feed.id}>
            <Link
              href={{
                pathname: '/[slug]',
                query: { slug: feed.slug },
              }}
            >
              <a>{feed.name}</a>
            </Link>
          </div>
        ))}
      </main>
    </div>
  )
}

const API_ENDPOINT_STATIC = 'http://live.orcasound.net/graphql'

const LIST_FEEDS = gql`
  {
    feeds {
      id
      name
      slug
      nodeName
      thumbUrl
      mapUrl
    }
  }
`

export async function getStaticProps() {
  const response = await request(API_ENDPOINT_STATIC, LIST_FEEDS)
  return { props: { feeds: response.feeds } }
}
