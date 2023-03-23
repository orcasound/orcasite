import gql from "graphql-tag"

export const SUBMIT_DETECTION = gql`
  mutation submitDetection(
    $feedId: ID!
    $playlistTimestamp: String!
    $playerOffset: Decimal!
    $description: String!
    $listenerCount: Int
  ) {
    submitDetection(
      feedId: $feedId
      playlistTimestamp: $playlistTimestamp
      playerOffset: $playerOffset
      listenerCount: $listenerCount
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
