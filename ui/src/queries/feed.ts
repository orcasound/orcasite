import { gql, GraphQLClient } from 'graphql-request'

import { getSdk } from '../generated/types'

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

export const api = getSdk(new GraphQLClient(API_ENDPOINT_STATIC))
