import gql from "graphql-tag"

export const CREATE_NOTIFICATION = gql`
mutation createNotificationEvent(
  $candidateId: ID! 
  $notifiedBy: String!
) {
  createNotificationEvent(
      candidateId: $candidateId
      notifiedBy: $notifiedBy
  ) {
    candidateId
    notifiedBy
  }
}
`