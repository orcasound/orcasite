import gql from "graphql-tag"

export const SUBMIT_DETECTION = gql`
  mutation submitDetection(
    $feedId: ID!
    $playlistTimestamp: String!
    $playerOffset: Decimal!
    $listenerCount: Int
  ) {
    submitDetection(
      feedId: $feedId
      playlistTimestamp: $playlistTimestamp
      playerOffset: $playerOffset
      listenerCount: $listenerCount
    ) {
      detection {
        id
      }
      lockoutInitial
      lockoutRemaining
    }
  }
`
