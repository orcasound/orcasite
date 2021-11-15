import Head from 'next/head'
import Link from 'next/link'

import { Feed } from '../generated/types'
import API from '../graphql/apiClient'

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
  const response = await API.feeds()
  return { props: { feeds: response.feeds } }
}
