defmodule OrcasiteWeb.TestSupport.GraphqlHelper do
  def register_mutation do
    """
      mutation registerWithPassword(
        $email: String!,
        $username: String!,
        $firstName: String,
        $lastName: String,
        $password: String!,
        $passwordConfirmation: String!
      ) {
        registerWithPassword(input: {
          email: $email,
          username: $username,
          firstName: $firstName,
          lastName: $lastName,
          password: $password,
          passwordConfirmation: $passwordConfirmation
        }) {
          errors {
            code
            message
            fields
          }
          result {
            id
            email
            admin
            moderator
            username
            firstName
            lastName
          }
        }
      }
    """
  end

  def sign_in_mutation do
    """
      mutation signInWithPassword($email: String!, $password: String!) {
      signInWithPassword(input: {
        email: $email,
        password: $password
      }) {
        errors {
          code
          message
          fields
        }
        user {
          id
          email
          admin
          moderator
        }
      }
      }
    """
  end

  def current_user_query do
    """
      query getCurrentUser {
        currentUser {
          id
          email
          admin
          moderator
        }
      }
    """
  end

  def sign_out_mutation do
    """
      mutation signOut {
        signOut
      }
    """
  end
end
