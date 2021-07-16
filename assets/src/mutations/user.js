import gql from "graphql-tag"

export const UPDATE_USER = gql`
mutation updateUser(
  $id: ID!
  $admin: Boolean!
) {
	updateUser(
    id: $id
    admin: $admin
  ) {
	  id
    admin
	} 
}`