import gql from 'graphql-tag'

export const logoutMutation = gql`
  mutation logoutCurrentUser {
    logout {
      id
    }
  }
`
