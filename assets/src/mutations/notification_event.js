import gql from "graphql-tag"

export const SUBMIT_DETECTION = gql`
  mutation createNotificationEvent(
    $candidateId: ID! 
    $notifiedBy: String!
) {
    createNotificationEvent(
        candidateId: $candidateId
        notifiedBy: $notifiedBy
    )
  }
`