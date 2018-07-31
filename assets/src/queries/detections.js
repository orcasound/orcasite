import gql from 'graphql-tag'

export const SUBMIT_DETECTION = gql`
mutation submitDetection($feed: Feed!, $playlistTimestamp: String!, $time: DateTime!) {
  submitDetection(feed: $feed, playlistTimestamp: $playlistTimestamp, time: $time ) {
    id
  }
}
`
