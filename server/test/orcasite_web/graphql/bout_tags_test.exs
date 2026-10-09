defmodule OrcasiteWeb.BoutTagsTest do
  @moduledoc """
  The bout tag mutations as the UI sends them, asking for the fields ItemTagParts.graphql
  asks for. If the server can't fill in one of those fields, the request fails with a 500,
  and tests that call the resource directly never notice.
  """
  use OrcasiteWeb.ConnCase

  import OrcasiteWeb.TestSupport.AuthenticationHelper,
    only: [create_user: 2, sign_in_user: 2]

  @moderator_params %{
    email: "moderator@example.com",
    password: "password",
    password_confirmation: "password"
  }

  @item_tag_parts """
  fragment ItemTagParts on ItemTag {
    id
    user { username }
    tag { id name slug description }
  }
  """

  setup %{conn: conn} do
    moderator = create_user(@moderator_params, moderator: true)
    feed = Orcasite.Generators.Radio.create_feed!()

    bout =
      Orcasite.Radio.Bout
      |> Ash.Changeset.for_create(
        :create,
        %{category: :biophony, start_time: DateTime.utc_now(), feed_id: feed.id},
        actor: moderator
      )
      |> Ash.create!()

    [conn: sign_in_user(conn, @moderator_params), bout: bout]
  end

  defp gql(conn, query, variables) do
    query = if query =~ "...ItemTagParts", do: query <> @item_tag_parts, else: query

    conn
    |> post("/graphql", %{"query" => query, "variables" => variables})
    |> json_response(200)
  end

  defp create(conn, bout, name) do
    gql(
      conn,
      """
      mutation ($boutId: ID!, $tagName: String!) {
        createBoutTag(input: { bout: { id: $boutId }, tag: { name: $tagName } }) {
          result { ...ItemTagParts }
          errors { message }
        }
      }
      """,
      %{"boutId" => bout.id, "tagName" => name}
    )
  end

  test "adding and removing a tag both return a result", %{conn: conn, bout: bout} do
    %{"data" => %{"createBoutTag" => %{"result" => created, "errors" => []}}} =
      create(conn, bout, "J")

    assert created["tag"]["name"] == "J"

    %{"data" => %{"deleteBoutTag" => %{"result" => deleted, "errors" => []}}} =
      gql(
        conn,
        """
        mutation ($id: ID!) {
          deleteBoutTag(id: $id) { result { id } errors { message } }
        }
        """,
        %{"id" => created["id"]}
      )

    assert deleted["id"] == created["id"]
  end

  test "a name whose slug is taken applies the existing tag without renaming it", %{
    conn: conn,
    bout: bout
  } do
    biggs =
      Orcasite.Radio.Tag
      |> Ash.Changeset.for_create(:create, %{name: "Bigg's"}, authorize?: false)
      |> Ash.create!(authorize?: false)

    %{"data" => %{"createBoutTag" => %{"result" => created}}} = create(conn, bout, "Biggs")

    assert created["tag"]["id"] == biggs.id
    assert created["tag"]["name"] == "Bigg's"
  end
end
