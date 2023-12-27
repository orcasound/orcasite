defmodule OrcasiteWeb.TestSupport.AuthenticationHelper do
  @endpoint OrcasiteWeb.Endpoint

  import Phoenix.ConnTest, only: [post: 3]

  import OrcasiteWeb.TestSupport.GraphqlHelper

  def create_user(params, opts \\ []) do
    moderator = Keyword.get(opts, :moderator)
    admin = Keyword.get(opts, :admin)

    strategy = AshAuthentication.Info.strategy!(Orcasite.Accounts.User, :password)

    user_changeset =
      Orcasite.Accounts.User
      |> Ash.Changeset.for_create(strategy.register_action_name, params)

    user_changeset =
      if moderator do
        Ash.Changeset.force_change_attribute(user_changeset, :moderator, true)
      else
        user_changeset
      end

    user_changeset =
      if admin do
        Ash.Changeset.force_change_attribute(user_changeset, :admin, true)
      else
        user_changeset
      end

    user_changeset |> Orcasite.Accounts.create()
  end

  def register_user(conn, %{
        email: email,
        password: password,
        first_name: first_name,
        last_name: last_name
      }) do
    post(conn, "/graphql", %{
      "query" => register_mutation(),
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
      "query" => sign_in_mutation(),
      "variables" => %{
        "email" => email,
        "password" => password
      }
    })
  end
end
