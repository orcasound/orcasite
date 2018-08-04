import gql from 'graphql-tag'

export const SUBMIT_DETECTION = gql`
mutation submitDetection($feedId: ID!, $playlistTimestamp: String!, $time: DateTime!) {
  submitDetection(feedId: $feedId, playlistTimestamp: $playlistTimestamp, time: $time ) {
    id
  }
}
`
