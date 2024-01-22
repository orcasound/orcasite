defmodule OrcasiteWeb.GraphqlTest.AccountsTest do
  use OrcasiteWeb.ConnCase, async: true

  import OrcasiteWeb.TestSupport.GraphqlHelper,
    only: [current_user_query: 0, sign_out_mutation: 0]

  import OrcasiteWeb.TestSupport.AuthenticationHelper, only: [register_user: 2, sign_in_user: 2]

  @user_params %{
    email: "nonadmin@example.com",
    username: "nonadmin",
    password: "password",
    password_confirmation: "password",
    first_name: "Non",
    last_name: "Admin"
  }

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
                 "user" => %{"email" => ^user_email, "admin" => false, "moderator" => false}
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
                 "user" => %{"email" => ^user_email, "admin" => false, "moderator" => false}
               }
             }
           } =
             json_response(conn, 200)

    conn = post(conn, "/graphql", %{"query" => current_user_query()})

    assert %{
             "data" => %{
               "currentUser" => %{"email" => ^user_email, "admin" => false, "moderator" => false}
             }
           } = json_response(conn, 200)

    conn =
      conn
      |> post("/graphql", %{"query" => sign_out_mutation()})
      |> post("/graphql", %{"query" => current_user_query()})

    assert %{"data" => %{"currentUser" => nil}} = json_response(conn, 200)
  end
end
