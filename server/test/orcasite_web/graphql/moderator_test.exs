defmodule OrcasiteWeb.ModeratorTest do
  use OrcasiteWeb.ConnCase

  import OrcasiteWeb.TestSupport.AuthenticationHelper,
    only: [create_user: 1, create_user: 2, sign_in_user: 2]

  @moderator_params %{
    email: "moderator@example.com",
    username: "moderator",
    password: "password",
    password_confirmation: "password"
  }

  @user_params %{
    email: "user@example.com",
    username: "user",
    password: "password",
    password_confirmation: "password"
  }

  setup do
    user = create_user(@user_params)
    moderator = create_user(@moderator_params, moderator: true)

    feed =
      Orcasite.Radio.Feed
      |> Ash.Changeset.for_create(
        :create,
        %{
          lat_lng_string: "48.5583362, -123.1735774",
          name: "Orcasound Lab (Haro Strait)",
          node_name: "rpi_orcasound_lab",
          bucket: "streaming-orcasound-net",
          slug: "orcasound-lab"
        }
      )
      |> Ash.create!(authorize?: false)

    detection =
      Orcasite.Radio.Detection.submit_detection(%{
        category: :other,
        feed_id: feed.id,
        playlist_timestamp: DateTime.to_unix(DateTime.utc_now()),
        player_offset: 5.54,
        description: "Test detection",
        listener_count: 1,
        send_notifications: false
      })
      |> Ash.load!(:candidate)

    [moderator: moderator, user: user, detection: detection, candidate: detection.candidate]
  end

  describe "moderator user" do
    test "can send notifications", %{conn: conn, candidate: candidate} do
      conn =
        sign_in_user(conn, @moderator_params)
        |> post("/graphql", %{
          "query" => """
            mutation {
              notifyConfirmedCandidate(
                input: {
                  candidateId: "#{candidate.id}",
                  message: "Test notification"
                }
              ) {
                result {
                  id
                }
              }
            }
          """
        })

      assert %{
               "data" => %{
                 "notifyConfirmedCandidate" => %{"result" => %{"id" => notification_id}}
               }
             } =
               json_response(conn, 200)

      candidate_id = candidate.id

      notification =
        Orcasite.Notifications.Notification |> Ash.get!(notification_id, authorize?: false)

      assert %{meta: %{"message" => "Test notification", "candidate_id" => ^candidate_id}} =
               notification
    end

    test "can view notificationsForCandidate", %{conn: conn, candidate: candidate} do
      conn =
        sign_in_user(conn, @moderator_params)
        |> post("/graphql", %{
          "query" => """
            query {
              notificationsForCandidate(candidateId: "#{candidate.id}") {
                id
              }
            }
          """
        })

      assert %{
               "data" => %{
                 "notificationsForCandidate" => []
               }
             } =
               json_response(conn, 200)
    end
  end

  describe "regular user" do
    test "cannot send notifications", %{conn: conn, candidate: candidate} do
      conn =
        sign_in_user(conn, @user_params)
        |> post("/graphql", %{
          "query" => """
            mutation {
              notifyConfirmedCandidate(
                input: {
                  candidateId: "#{candidate.id}",
                  message: "Test notification"
                }
              ) {
                errors {
                  code
                }
                result {
                  id
                }
              }
            }
          """
        })

      assert %{
               "data" => %{
                 "notifyConfirmedCandidate" => %{"errors" => [%{"code" => "forbidden"}]}
               }
             } = json_response(conn, 200)
    end

    test "cannot view notificationsForCandidate", %{conn: conn, candidate: candidate} do
      conn =
        sign_in_user(conn, @user_params)
        |> post("/graphql", %{
          "query" => """
            query {
              notificationsForCandidate(candidateId: "#{candidate.id}") {
                id
              }
            }
          """
        })

      assert %{"errors" => [%{"code" => "forbidden"}]} = json_response(conn, 200)
    end
  end
end
