defmodule OrcasiteWeb.RadioTest do
  use OrcasiteWeb.ConnCase

  setup do
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

    [feed: feed]
  end

  test "unauthenticated listeners can submit a detection", %{conn: conn, feed: feed} do
    conn =
      conn
      |> post("/graphql", %{
        "query" => """
          mutation submitDetection(
            $feedId: String!
            $playlistTimestamp: Int!
            $playerOffset: Decimal!
            $description: String!
            $listenerCount: Int
            $category: DetectionCategory!
          ) {
            submitDetection(
              input: {
                feedId: $feedId
                playlistTimestamp: $playlistTimestamp
                playerOffset: $playerOffset
                listenerCount: $listenerCount
                description: $description
                category: $category
                sendNotifications: false
              }
            ) {
              result {
                id
              }
              errors {
                message
                code
                fields
                shortMessage
                vars
              }
            }
          }
        """,
        "variables" => %{
          "feedId" => feed.id,
          "playlistTimestamp" => DateTime.to_unix(DateTime.utc_now()),
          "playerOffset" => 5.54,
          "description" => "Test detection",
          "listenerCount" => 1,
          "category" => "OTHER"
        }
      })

    assert %{"data" => %{"submitDetection" => %{"errors" => [], "result" => %{"id" => id}}}} =
             json_response(conn, 200)

    assert "det_" <> _ = id
  end

  describe "feeds query" do
    test "succeeds for public fields", %{conn: conn, feed: %{id: feed_id, slug: feed_slug}} do
      assert %{
               "data" => %{
                 "feeds" => [%{"id" => ^feed_id, "slug" => ^feed_slug}]
               }
             } =
               conn
               |> post("/graphql", %{
                 "query" => """
                 {
                   feeds {
                     id
                     slug
                   }
                 }
                 """
               })
               |> json_response(200)
    end

    test "succeeds for forbidden fields", %{conn: conn, feed: %{id: feed_id, slug: feed_slug}} do
      assert %{
               "data" => %{
                 "feeds" => [
                   %{
                     "id" => ^feed_id,
                     "maintainerEmails" => nil,
                     "slug" => ^feed_slug
                   }
                 ]
               },
               "errors" => [
                 %{
                   "code" => "forbidden_field",
                   "fields" => [],
                   "locations" => [%{"column" => 5, "line" => 5}],
                   "message" => "forbidden field",
                   "path" => ["feeds", 0, "maintainerEmails"],
                   "short_message" => "forbidden field",
                   "vars" => %{}
                 }
               ]
             } =
               conn
               |> post("/graphql", %{
                 "query" => """
                 {
                   feeds {
                     id
                     slug
                     maintainerEmails
                   }
                 }
                 """
               })
               |> json_response(200)
    end
  end
end
