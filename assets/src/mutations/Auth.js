import gql from 'graphql-tag'

export const logoutMutation = gql`
  mutation LogoutCurrentUser {
    logout {
      id
    }
  }
`
