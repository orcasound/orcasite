defmodule Orcasite.Radio.TagTest do
  use Orcasite.DataCase, async: true

  alias Orcasite.Radio.Tag

  setup do
    [moderator: Orcasite.Generators.Accounts.create_user!(moderator: true)]
  end

  defp create_tag!(name, actor) do
    Tag
    |> Ash.Changeset.for_create(:create, %{name: name}, actor: actor)
    |> Ash.create!()
  end

  # The fields a rejected change was rejected on, so a test fails if the change is
  # refused for some other reason (authorization, say) than the one it names.
  defp rejected_fields({:error, %Ash.Error.Invalid{errors: errors}}),
    do:
      errors
      |> Enum.flat_map(&List.wrap(Map.get(&1, :field) || Map.get(&1, :fields)))
      |> Enum.uniq()

  defp classify(tag, attrs, actor) do
    tag
    |> Ash.Changeset.for_update(:update, attrs, actor: actor)
    |> Ash.update()
  end

  describe "kind and iri" do
    test "a new tag is unclassified: free text stays legal", %{moderator: moderator} do
      tag = create_tag!("J", moderator)

      assert tag.kind == nil
      assert tag.iri == nil
    end

    test "a tag can be classified and cite an identifier", %{moderator: moderator} do
      {:ok, tag} =
        "J" |> create_tag!(moderator) |> classify(%{kind: :animal, iri: "SSA:0000020"}, moderator)

      assert tag.kind == :animal
      assert tag.iri == "SSA:0000020"
    end

    test "kind is one of animal, signal or other", %{moderator: moderator} do
      assert [:kind] ==
               "J"
               |> create_tag!(moderator)
               |> classify(%{kind: :pod}, moderator)
               |> rejected_fields()
    end

    test "any number of tags may have no iri", %{moderator: moderator} do
      {:ok, _} = "S01" |> create_tag!(moderator) |> classify(%{kind: :signal}, moderator)
      {:ok, _} = "S04" |> create_tag!(moderator) |> classify(%{kind: :signal}, moderator)
    end

    test "two tags cannot cite the same identifier", %{moderator: moderator} do
      {:ok, _} = "J" |> create_tag!(moderator) |> classify(%{iri: "SSA:0000020"}, moderator)

      assert [:iri] ==
               "J pod"
               |> create_tag!(moderator)
               |> classify(%{iri: "SSA:0000020"}, moderator)
               |> rejected_fields()
    end

    test "an iri is an identifier, not prose", %{moderator: moderator} do
      assert [:iri] ==
               "J"
               |> create_tag!(moderator)
               |> classify(%{iri: "J pod"}, moderator)
               |> rejected_fields()
    end

    test "re-tagging by name does not erase a classification", %{moderator: moderator} do
      {:ok, _} =
        "J" |> create_tag!(moderator) |> classify(%{kind: :animal, iri: "SSA:0000020"}, moderator)

      # What happens every time a moderator applies an existing tag to a bout.
      again = create_tag!("J", moderator)

      assert again.kind == :animal
      assert again.iri == "SSA:0000020"
    end
  end

  describe "names" do
    test "creating a tag whose name differs only by case reuses the existing tag", %{
      moderator: moderator
    } do
      create_tag!("Humpback", moderator)

      assert create_tag!("humpback", moderator).slug == "humpback"
      assert [_only_one] = Ash.read!(Tag, actor: moderator)
    end

    test "a tag cannot be renamed into a name that differs only by case", %{
      moderator: moderator
    } do
      create_tag!("Humpback", moderator)

      assert [:name] ==
               "Megaptera"
               |> create_tag!(moderator)
               |> classify(%{name: "HUMPBACK"}, moderator)
               |> rejected_fields()
    end
  end
end
