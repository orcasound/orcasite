import gql from 'graphql-tag'

export const LIST_FEEDS = gql`
{
  feeds {
    id
    name
    slug
    nodeName
    locationPoint
  }
}
`

export const GET_FEED = gql`
{
  feed(id: $id) {
    id
    name
    slug
    nodeName
    locationPoint
  }
}
`
