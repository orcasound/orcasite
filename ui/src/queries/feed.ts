import { gql, request } from 'graphql-request'

// TODO: get API URL from env
const API_ENDPOINT_STATIC = 'http://live.orcasound.net/graphql'

const LIST_FEEDS = gql`
  query feeds {
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

const GET_FEED = gql`
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

export function listFeeds() {
  return request(API_ENDPOINT_STATIC, LIST_FEEDS)
}

export function getFeed(slug: string) {
  return request(API_ENDPOINT_STATIC, GET_FEED, {
    slug: slug,
  })
}
