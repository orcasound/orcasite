import gql from "graphql-tag"

export const CURRENT_USER = gql`
  {
    currentUser {
      id
      email
    }
  }
`

export const LIST_USERS = gql`
  query getUsers($pagination: Pagination!) {
    users(pagination: $pagination) {
      meta {
        currentPage
        previousPage
        nextPage
        totalEntries
        totalPages
      }
      entries {
        id
        email
        admin
      }
    }
  }
`
