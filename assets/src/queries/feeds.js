import gql from "graphql-tag"

export const LIST_FEEDS = gql`
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

export const GET_FEED = gql`
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
`
