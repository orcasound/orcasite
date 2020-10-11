import gql from "graphql-tag"

export const SUBMIT_DETECTION = gql`
  mutation submitDetection(
    $feedId: ID!
    $playlistTimestamp: String!
    $playerOffset: Decimal!
    $description: String!
    $type: String!
  ) {
    submitDetection(
      feedId: $feedId
      playlistTimestamp: $playlistTimestamp
      playerOffset: $playerOffset
      type: $type
      description: $description
    ) {
      detection {
        id
      }
      lockoutInitial
      lockoutRemaining
    }
  }
`
