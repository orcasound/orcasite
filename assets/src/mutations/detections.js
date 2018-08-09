import gql from 'graphql-tag'

export const SUBMIT_DETECTION = gql`
mutation submitDetection($feedId: ID!, $playlistTimestamp: String!, $playerOffset: Decimal!) {
  submitDetection(feedId: $feedId, playlistTimestamp: $playlistTimestamp, playerOffset: $playerOffset ) {
    detection {
      id
    }
    lockoutInitial
    lockoutRemaining
  }
}
`
