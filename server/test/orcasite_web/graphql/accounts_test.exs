defmodule OrcasiteWeb.AccountsTest do
  use OrcasiteWeb.ConnCase, async: true

  @user_params %{
    email: "nonadmin@example.com",
    password: "password",
    password_confirmation: "password",
    first_name: "Non",
    last_name: "Admin"
  }

  @register_mutation """
    mutation registerWithPassword(
      $email: String!,
      $firstName: String,
      $lastName: String,
      $password: String!,
      $passwordConfirmation: String!
    ) {
      registerWithPassword(input: {
        email: $email,
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
        }
      }
    }
  """

  @sign_in_mutation """
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

  @current_user_query """
    query getCurrentUser {
      currentUser {
        id
        email
        admin
        moderator
      }
    }
  """

  @sign_out_mutation """
    mutation signOut {
      signOut
    }
  """

  test "graphql returns 200", %{conn: conn} do
    conn =
      post(conn, "/graphql", %{
        "query" => """
        query {
          __schema {
            types {
              name
            }
          }
        }
        """
      })

    assert %{"data" => _} = json_response(conn, 200)
  end

  test "registers new user", %{conn: conn} do
    conn = register_user(conn, @user_params)
    user_email = @user_params[:email]

    assert %{"data" => %{"registerWithPassword" => %{"result" => %{"email" => ^user_email}}}} =
             json_response(conn, 200)
  end

  test "signs in with user", %{conn: conn} do
    conn =
      register_user(conn, @user_params)
      |> sign_in_user(@user_params)

    user_email = @user_params[:email]

    assert %{
             "data" => %{
               "signInWithPassword" => %{
                 "user" => %{"email" => ^user_email, "admin" => nil, "moderator" => nil}
               }
             }
           } =
             json_response(conn, 200)
  end

  test "signs out with user", %{conn: conn} do
    conn =
      register_user(conn, @user_params)
      |> sign_in_user(@user_params)

    user_email = @user_params[:email]

    assert %{
             "data" => %{
               "signInWithPassword" => %{
                 "user" => %{"email" => ^user_email, "admin" => nil, "moderator" => nil}
               }
             }
           } =
             json_response(conn, 200)

    conn = post(conn, "/graphql", %{"query" => @current_user_query})

    assert %{
             "data" => %{
               "currentUser" => %{"email" => ^user_email, "admin" => nil, "moderator" => nil}
             }
           } = json_response(conn, 200)

    conn =
      conn
      |> post("/graphql", %{"query" => @sign_out_mutation})
      |> post("/graphql", %{"query" => @current_user_query})

    assert %{"data" => %{"currentUser" => nil}} = json_response(conn, 200)
  end

  def register_user(conn, %{
        email: email,
        password: password,
        first_name: first_name,
        last_name: last_name
      }) do
    post(conn, "/graphql", %{
      "query" => @register_mutation,
      "variables" => %{
        "email" => email,
        "password" => password,
        "passwordConfirmation" => password,
        "firstName" => first_name,
        "lastName" => last_name
      }
    })
  end

  def sign_in_user(conn, %{email: email, password: password}) do
    post(conn, "/graphql", %{
      "query" => @sign_in_mutation,
      "variables" => %{
        "email" => email,
        "password" => password
      }
    })
  end
end
