defmodule OrcasiteWeb.ModeratorTest do
  use OrcasiteWeb.ConnCase, async: true

  import OrcasiteWeb.TestSupport.AuthenticationHelper

  @moderator_params %{
    email: "moderator@example.com",
    password: "password",
    password_confirmation: "password"
  }

  @user_params %{
    email: "user@example.com",
    password: "password",
    password_confirmation: "password"
  }

  setup_all do
    strategy = AshAuthentication.Info.strategy!(Orcasite.Accounts.User, :password)

    user = create_user(@user_params)
    moderator = create_user(@moderator_params, moderator: true)

    [moderator: moderator, user: user]
  end

  describe "regular user" do
    # test "does not see notificationsForCandidate", %{conn: conn} do
    #   conn =
    #     post(conn, "/graphql", %{
    #       "query" => """
    #         query {
    #           notificationsForCandidate {
    #             id
    #           }
    #         }
    #       """
    #     })
    # end
  end
end
