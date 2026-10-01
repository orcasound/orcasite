defmodule OrcasiteWeb.JsonApi.BoutsTest do
  use OrcasiteWeb.ConnCase, async: true

  setup do
    feed = Orcasite.Generators.Radio.create_feed!()
    moderator = Orcasite.Generators.Accounts.create_user!(moderator: true)

    bout =
      Orcasite.Radio.Bout
      |> Ash.Changeset.for_create(
        :create,
        %{
          category: :biophony,
          start_time: DateTime.utc_now(),
          feed_id: feed.id
        },
        actor: moderator
      )
      |> Ash.create!()

    Orcasite.Radio.ItemTag
    |> Ash.Changeset.for_create(
      :bout_tag,
      %{
        tag: %{name: "seagull", description: "Sounds like a seagull"},
        bout: %{id: bout.id}
      },
      actor: moderator
    )
    |> Ash.create!()

    [feed: feed, bout: bout]
  end

  describe "GET /api/json/bouts" do
    test "includes a JSON:API type for tags", %{conn: conn, feed: feed} do
      feed_id = feed.id

      response =
        conn
        |> put_req_header("accept", "application/vnd.api+json")
        |> get("/api/json/bouts", %{"include" => "feed,tags"})
        |> json_response(200)

      assert [%{"relationships" => relationships}] = response["data"]

      assert [%{"type" => "tag", "id" => tag_id}] = relationships["tags"]["data"]
      assert %{"type" => "feed", "id" => ^feed_id} = relationships["feed"]["data"]

      assert %{"type" => "tag", "attributes" => %{"name" => "seagull", "slug" => "seagull"}} =
               Enum.find(response["included"], &(&1["id"] == tag_id))
    end

    test "an included tag says what kind of thing it names and which identifier it cites", %{
      conn: conn
    } do
      Orcasite.Radio.Tag
      |> Ash.read_one!(authorize?: false)
      |> Ash.Changeset.for_update(:update, %{kind: :animal, iri: "SSA:0000908"},
        authorize?: false
      )
      |> Ash.update!()

      response =
        conn
        |> put_req_header("accept", "application/vnd.api+json")
        |> get("/api/json/bouts", %{"include" => "tags"})
        |> json_response(200)

      assert [%{"attributes" => %{"kind" => "animal", "iri" => "SSA:0000908"}}] =
               Enum.filter(response["included"], &(&1["type"] == "tag"))
    end

    test "an included item tag carries the moderator's certainty beside the tag it applies", %{
      conn: conn,
      bout: bout
    } do
      moderator = Orcasite.Generators.Accounts.create_user!(moderator: true)

      Orcasite.Radio.ItemTag
      |> Ash.Changeset.for_create(
        :bout_tag,
        %{
          tag: %{name: "L", description: "L pod"},
          bout: %{id: bout.id},
          certainty: :possible
        },
        actor: moderator
      )
      |> Ash.create!()

      response =
        conn
        |> put_req_header("accept", "application/vnd.api+json")
        |> get("/api/json/bouts", %{"include" => "item_tags.tag"})
        |> json_response(200)

      item_tags = Enum.filter(response["included"], &(&1["type"] == "item_tag"))
      tags = Enum.filter(response["included"], &(&1["type"] == "tag"))

      assert length(item_tags) == 2
      assert length(tags) == 2

      by_tag_name = fn name ->
        tag = Enum.find(tags, &(&1["attributes"]["name"] == name))
        Enum.find(item_tags, &(&1["relationships"]["tag"]["data"]["id"] == tag["id"]))
      end

      # The hedge survives, and an application made without being asked is nil, not certain.
      assert %{"attributes" => %{"certainty" => "possible"}} = by_tag_name.("L")
      assert %{"attributes" => %{"certainty" => nil}} = by_tag_name.("seagull")
    end

    test "an unclassified tag is included with a null kind and iri", %{conn: conn} do
      response =
        conn
        |> put_req_header("accept", "application/vnd.api+json")
        |> get("/api/json/bouts", %{"include" => "tags"})
        |> json_response(200)

      assert [%{"attributes" => %{"name" => "seagull", "kind" => nil, "iri" => nil}}] =
               Enum.filter(response["included"], &(&1["type"] == "tag"))
    end
  end

  describe "POST /api/json/bouts" do
    setup %{feed: feed} do
      [
        params: %{
          feed_id: feed.id,
          category: "biophony",
          name: "Seagulls",
          start_time: "2026-09-30T18:00:00Z",
          end_time: "2026-09-30T18:15:30Z"
        }
      ]
    end

    test "a moderator's API key creates a bout on the feed, attributed to them", %{
      conn: conn,
      feed: feed,
      params: params
    } do
      moderator = Orcasite.Generators.Accounts.create_user!(moderator: true)
      feed_id = feed.id

      response = conn |> post_bout(params, api_key_for(moderator)) |> json_response(201)

      assert %{
               "type" => "bout",
               "id" => "bout_" <> _ = bout_id,
               "attributes" => %{
                 "category" => "biophony",
                 "name" => "Seagulls",
                 "start_time" => "2026-09-30T18:00:00.000000Z",
                 "end_time" => "2026-09-30T18:15:30.000000Z",
                 "duration" => duration
               }
             } = response["data"]

      assert Decimal.equal?(Decimal.new("930"), Decimal.new(to_string(duration)))

      bout =
        Orcasite.Radio.Bout
        |> Ash.get!(bout_id, authorize?: false)
        |> Ash.load!(:created_by_user, authorize?: false)

      assert bout.feed_id == feed_id
      assert bout.created_by_user.id == moderator.id
    end

    test "a non-moderator's API key is forbidden", %{conn: conn, params: params} do
      user = Orcasite.Generators.Accounts.create_user!()

      capture_log(fn ->
        assert %{"errors" => [%{"code" => "forbidden"}]} =
                 conn |> post_bout(params, api_key_for(user)) |> json_response(403)
      end)
    end

    test "a request without an API key is forbidden", %{conn: conn, params: params} do
      capture_log(fn ->
        assert %{"errors" => [%{"code" => "forbidden"}]} =
                 conn |> post_bout(params) |> json_response(403)
      end)
    end

    test "is listed in the OpenAPI spec that Swagger UI renders", %{conn: conn} do
      # Served without a content-type, so json_response/2 won't take it
      spec = conn |> get("/api/json/open_api") |> response(200) |> Jason.decode!()

      assert %{"post" => _, "get" => _} = spec["paths"]["/api/json/bouts"]
    end

    test "a bout without a feed is rejected", %{conn: conn, params: params} do
      moderator = Orcasite.Generators.Accounts.create_user!(moderator: true)

      capture_log(fn ->
        assert %{"errors" => [_ | _]} =
                 conn
                 |> post_bout(Map.delete(params, :feed_id), api_key_for(moderator))
                 |> json_response(400)
      end)
    end
  end

  describe "PATCH /api/json/bouts/:id" do
    setup %{feed: feed} do
      bout =
        Orcasite.Radio.Bout
        |> Ash.Changeset.for_create(
          :create,
          %{
            feed_id: feed.id,
            category: :biophony,
            name: "Seagulls",
            start_time: ~U[2026-09-30 18:00:00Z],
            end_time: ~U[2026-09-30 18:15:30Z]
          },
          authorize?: false
        )
        |> Ash.create!()

      [bout: bout]
    end

    test "a moderator's API key corrects the end time, and the duration follows", %{
      conn: conn,
      bout: bout
    } do
      moderator = Orcasite.Generators.Accounts.create_user!(moderator: true)
      bout_id = bout.id

      response =
        conn
        |> patch_bout(bout, %{end_time: "2026-09-30T18:20:00Z"}, api_key_for(moderator))
        |> json_response(200)

      assert %{
               "type" => "bout",
               "id" => ^bout_id,
               "attributes" => %{
                 "name" => "Seagulls",
                 "start_time" => "2026-09-30T18:00:00.000000Z",
                 "end_time" => "2026-09-30T18:20:00.000000Z",
                 "duration" => duration
               }
             } = response["data"]

      assert Decimal.equal?(Decimal.new("1200"), Decimal.new(to_string(duration)))

      bout = Ash.get!(Orcasite.Radio.Bout, bout_id, authorize?: false)
      assert bout.end_time == ~U[2026-09-30 18:20:00.000000Z]
      assert Decimal.equal?(Decimal.new("1200"), Decimal.new(to_string(bout.duration)))
    end

    test "a moderator's API key corrects the start time, and the duration follows", %{
      conn: conn,
      bout: bout
    } do
      moderator = Orcasite.Generators.Accounts.create_user!(moderator: true)

      response =
        conn
        |> patch_bout(bout, %{start_time: "2026-09-30T18:05:00Z"}, api_key_for(moderator))
        |> json_response(200)

      assert %{
               "start_time" => "2026-09-30T18:05:00.000000Z",
               "end_time" => "2026-09-30T18:15:30.000000Z",
               "duration" => duration
             } = response["data"]["attributes"]

      assert Decimal.equal?(Decimal.new("630"), Decimal.new(to_string(duration)))
    end

    test "a non-moderator's API key is forbidden and the bout is unchanged", %{
      conn: conn,
      bout: bout
    } do
      user = Orcasite.Generators.Accounts.create_user!()

      capture_log(fn ->
        assert %{"errors" => [%{"code" => "forbidden"}]} =
                 conn
                 |> patch_bout(bout, %{end_time: "2026-09-30T18:20:00Z"}, api_key_for(user))
                 |> json_response(403)
      end)

      assert Ash.get!(Orcasite.Radio.Bout, bout.id, authorize?: false).end_time ==
               bout.end_time
    end

    test "a request without an API key is forbidden", %{conn: conn, bout: bout} do
      capture_log(fn ->
        assert %{"errors" => [%{"code" => "forbidden"}]} =
                 conn
                 |> patch_bout(bout, %{end_time: "2026-09-30T18:20:00Z"})
                 |> json_response(403)
      end)
    end

    test "is listed in the OpenAPI spec that Swagger UI renders", %{conn: conn} do
      spec = conn |> get("/api/json/open_api") |> response(200) |> Jason.decode!()

      assert %{"patch" => _} = spec["paths"]["/api/json/bouts/{id}"]
    end
  end

  defp api_key_for(user) do
    Orcasite.Accounts.ApiKey.create!(%{user_id: user.id}, authorize?: false).__metadata__.plaintext_api_key
  end

  defp post_bout(conn, params, api_key \\ nil) do
    conn =
      conn
      |> put_req_header("content-type", "application/vnd.api+json")
      |> put_req_header("accept", "application/vnd.api+json")

    conn =
      if api_key, do: put_req_header(conn, "authorization", "Bearer #{api_key}"), else: conn

    post(conn, "/api/json/bouts", %{data: %{type: "bout", attributes: params}})
  end

  defp patch_bout(conn, bout, attributes, api_key \\ nil) do
    conn =
      conn
      |> put_req_header("content-type", "application/vnd.api+json")
      |> put_req_header("accept", "application/vnd.api+json")

    conn =
      if api_key, do: put_req_header(conn, "authorization", "Bearer #{api_key}"), else: conn

    patch(conn, "/api/json/bouts/#{bout.id}", %{
      data: %{type: "bout", id: bout.id, attributes: attributes}
    })
  end
end
