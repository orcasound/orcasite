import { graphql } from '../generated'

export const LIST_FEEDS = graphql(`
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

export const GET_FEED = graphql(`
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
