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
end
