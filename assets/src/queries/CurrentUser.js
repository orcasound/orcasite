import gql from 'graphql-tag'

export const currentUser = gql`
{
  currentUser {
    id
    email
  }
}`
