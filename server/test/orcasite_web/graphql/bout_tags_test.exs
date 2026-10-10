defmodule OrcasiteWeb.BoutTagsTest do
  @moduledoc """
  The bout tag mutations as the picker sends them (#1015), asking for the fields
  ItemTagParts.graphql asks for. If the server can't fill in one of those fields, the
  request fails with a 500, and tests that call the resource directly never notice.
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
    certainty
    userId
    user { username }
    tag { id name slug description kind iri }
  }
  """

  setup %{conn: conn} do
    # no username, like the seeded admin
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

    [conn: sign_in_user(conn, @moderator_params), bout: bout, moderator: moderator]
  end

  defp gql(conn, query, variables) do
    conn
    |> post("/graphql", %{"query" => with_fragment(query), "variables" => variables})
    |> json_response(200)
  end

  defp with_fragment(query) do
    if String.contains?(query, "...ItemTagParts"), do: query <> @item_tag_parts, else: query
  end

  defp create(conn, bout, tag) do
    gql(
      conn,
      """
      mutation ($boutId: ID!, $tagName: String!, $tagKind: String, $tagIri: String) {
        createBoutTag(input: {
          bout: { id: $boutId }
          tag: { name: $tagName, kind: $tagKind, iri: $tagIri }
          certainty: "certain"
        }) {
          result { ...ItemTagParts }
          errors { message }
        }
      }
      """,
      Map.merge(%{"boutId" => bout.id}, tag)
    )
  end

  test "applying a tag, setting its certainty and removing it all return a result", %{
    conn: conn,
    bout: bout,
    moderator: moderator
  } do
    %{"data" => %{"createBoutTag" => %{"result" => created, "errors" => []}}} =
      create(conn, bout, %{"tagName" => "J pod", "tagKind" => "animal", "tagIri" => "SSA:0000020"})

    assert created["userId"] == moderator.id
    assert created["tag"]["iri"] == "SSA:0000020"

    %{"data" => %{"setBoutTagCertainty" => %{"result" => hedged}}} =
      gql(
        conn,
        """
        mutation ($id: ID!) {
          setBoutTagCertainty(id: $id, input: { certainty: "probable" }) {
            result { ...ItemTagParts }
            errors { message }
          }
        }
        """,
        %{"id" => created["id"]}
      )

    assert hedged["certainty"] == "probable"

    %{"data" => %{"deleteBoutTag" => %{"result" => deleted, "errors" => []}}} =
      gql(
        conn,
        """
        mutation ($id: ID!) {
          deleteBoutTag(id: $id) {
            result { id }
            errors { message }
          }
        }
        """,
        %{"id" => created["id"]}
      )

    assert deleted["id"] == created["id"]
  end

  test "a refused request returns errors and no result", %{conn: conn, bout: bout} do
    Orcasite.Radio.Tag
    |> Ash.Changeset.for_create(:seed, %{name: "call", kind: :signal}, authorize?: false)
    |> Ash.create!(authorize?: false)

    assert %{"data" => %{"createBoutTag" => %{"result" => nil, "errors" => [_ | _]}}} =
             create(conn, bout, %{"tagName" => "call", "tagKind" => "other"})
  end

  test "a name whose slug is taken applies the existing tag without renaming it", %{
    conn: conn,
    bout: bout
  } do
    biggs =
      Orcasite.Radio.Tag
      |> Ash.Changeset.for_create(:create, %{name: "Bigg's"}, authorize?: false)
      |> Ash.create!(authorize?: false)

    %{"data" => %{"createBoutTag" => %{"result" => created}}} =
      create(conn, bout, %{"tagName" => "Biggs"})

    assert created["tag"]["id"] == biggs.id
    assert created["tag"]["name"] == "Bigg's"
  end
end
