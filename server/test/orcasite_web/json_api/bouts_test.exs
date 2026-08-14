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
  end
end
