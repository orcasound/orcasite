import Head from 'next/head'
import Link from 'next/link'

import { Feed } from '../generated/types'
import { listFeeds } from '../queries/feed'

export default function HomePage({ feeds }: { feeds: Feed[] }) {
  return (
    <div>
      <Head>
        <title>Orcasound</title>
      </Head>

      <main>
        {feeds.map((feed) => (
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

export async function getStaticProps() {
  const response = await listFeeds()
  return { props: { feeds: response.feeds } }
}
