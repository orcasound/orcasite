defmodule OrcasiteWeb.JsonApi.DetectionsTest do
  use OrcasiteWeb.ConnCase, async: true

  setup do
    feed = Orcasite.Generators.Radio.create_feed!()
    Orcasite.Generators.Radio.create_feed_stream!(feed_id: feed.id)
    bot_user = Orcasite.Generators.Accounts.create_user!(detection_bot: true)

    api_key =
      Orcasite.Accounts.ApiKey.create!(%{user_id: bot_user.id}, authorize?: false)

    [bot_user: bot_user, api_key: api_key.__metadata__.plaintext_api_key, feed: feed]
  end

  describe "detections api machine submission" do
    test "succeeds with bot user", %{conn: conn, api_key: api_key, feed: feed} do
      params = %{
        data: %{
          attributes: %{
            timestamp: DateTime.to_iso8601(DateTime.utc_now()),
            description: "Description",
            feed_id: feed.id,
            idempotency_key: "idempotency"
          }
        }
      }

      capture_log(fn ->
        assert %{
                 "data" => %{
                   "attributes" => %{
                     "category" => "whale",
                     "description" => "Description",
                     "inserted_at" => _,
                     "listener_count" => 0,
                     "player_offset" => _,
                     "playlist_timestamp" => _,
                     "timestamp" => _,
                     "source" => "machine"
                   },
                   "id" => _,
                   "links" => %{},
                   "meta" => %{},
                   "relationships" => %{
                     "candidate" => %{"links" => %{}, "meta" => %{}},
                     "feed" => %{"links" => %{}, "meta" => %{}}
                   },
                   "type" => "detection"
                 },
                 "jsonapi" => %{"version" => "1.0"},
                 "links" => %{"self" => "http://www.example.com/api/json/detections"},
                 "meta" => %{}
               } =
                 conn
                 |> put_req_header("content-type", "application/vnd.api+json")
                 |> put_req_header("authorization", "Bearer #{api_key}")
                 |> post("/api/json/detections", params)
                 |> json_response(201)
      end)

      updated_params = %{
        data: %{
          attributes: %{
            timestamp: DateTime.to_iso8601(DateTime.utc_now() |> DateTime.add(-1, :minute)),
            feed_id: feed.id,
            description: "New description",
            idempotency_key: "idempotency"
          }
        }
      }

      capture_log(fn ->
        assert %{
                 "data" => %{
                   "attributes" => %{
                     "category" => "whale",
                     "description" => "New description",
                     "inserted_at" => _,
                     "listener_count" => 0,
                     "player_offset" => _,
                     "playlist_timestamp" => _,
                     "timestamp" => _,
                     "source" => "machine"
                   },
                   "id" => _,
                   "links" => %{},
                   "meta" => %{},
                   "relationships" => %{
                     "candidate" => %{"links" => %{}, "meta" => %{}},
                     "feed" => %{"links" => %{}, "meta" => %{}}
                   },
                   "type" => "detection"
                 },
                 "jsonapi" => %{"version" => "1.0"},
                 "links" => %{"self" => "http://www.example.com/api/json/detections"},
                 "meta" => %{}
               } =
                 conn
                 |> put_req_header("content-type", "application/vnd.api+json")
                 |> put_req_header("authorization", "Bearer #{api_key}")
                 |> post("/api/json/detections", updated_params)
                 |> json_response(201)
      end)
    end

    test "fails without user", %{conn: conn, feed: feed} do
      capture_log(fn ->
        assert %{
                 "errors" => [
                   %{
                     "code" => "forbidden",
                     "detail" => "forbidden",
                     "id" => _,
                     "status" => "403",
                     "title" => "Forbidden"
                   }
                 ],
                 "jsonapi" => %{"version" => "1.0"}
               } =
                 conn
                 |> put_req_header("content-type", "application/vnd.api+json")
                 |> post("/api/json/detections", %{
                   data: %{
                     attributes: %{
                       timestamp: DateTime.to_iso8601(DateTime.utc_now()),
                       feed_id: feed.id
                     }
                   }
                 })
                 |> json_response(403)
      end)
    end
  end
end
