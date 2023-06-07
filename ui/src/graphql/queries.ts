import { graphql } from '@/graphql/generated'

export const feedsQuery = graphql(`
  query feeds {
    feeds {
      id
      name
      slug
      nodeName
      locationPoint
      thumbUrl
      mapUrl
    }
  }
`)

export const feedQuery = graphql(`
  query feed($slug: String!) {
    feed(slug: $slug) {
      id
      name
      slug
      nodeName
      locationPoint
      introHtml
      thumbUrl
      mapUrl
    }
  }
`)
