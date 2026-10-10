defmodule Orcasite.Radio.ItemTagTest do
  use Orcasite.DataCase, async: true

  alias Orcasite.Radio.ItemTag

  setup do
    feed = Orcasite.Generators.Radio.create_feed!()
    moderator = Orcasite.Generators.Accounts.create_user!(moderator: true)

    bout =
      Orcasite.Radio.Bout
      |> Ash.Changeset.for_create(
        :create,
        %{category: :biophony, start_time: DateTime.utc_now(), feed_id: feed.id},
        actor: moderator
      )
      |> Ash.create!()

    [bout: bout, moderator: moderator]
  end

  defp apply_tag(bout, moderator, attrs) do
    ItemTag
    |> Ash.Changeset.for_create(
      :bout_tag,
      Map.merge(%{tag: %{name: "L", description: "L pod"}, bout: %{id: bout.id}}, attrs),
      actor: moderator
    )
    |> Ash.create()
  end

  describe "certainty on a tag application" do
    test "is recorded as the moderator gave it", %{bout: bout, moderator: moderator} do
      assert {:ok, %ItemTag{certainty: :possible}} =
               apply_tag(bout, moderator, %{certainty: :possible})
    end

    test "is nil when nobody was asked, never defaulted to certain", %{
      bout: bout,
      moderator: moderator
    } do
      assert {:ok, %ItemTag{certainty: nil}} = apply_tag(bout, moderator, %{})
    end

    test "accepts only the three words", %{bout: bout, moderator: moderator} do
      assert {:error, %Ash.Error.Invalid{errors: [%{field: :certainty}]}} =
               apply_tag(bout, moderator, %{certainty: :maybe})

      assert {:error, %Ash.Error.Invalid{errors: [%{field: :certainty}]}} =
               apply_tag(bout, moderator, %{certainty: 0.5})
    end

    test "can be revised by the moderator, which changes the claim in place", %{
      bout: bout,
      moderator: moderator
    } do
      {:ok, item_tag} = apply_tag(bout, moderator, %{certainty: :possible})

      revised =
        item_tag
        |> Ash.Changeset.for_update(:update, %{certainty: :certain}, actor: moderator)
        |> Ash.update!()

      assert revised.id == item_tag.id
      assert revised.certainty == :certain
    end
  end

  describe "the tag a picked name applies (#1015)" do
    defp tag!(attrs) do
      Orcasite.Radio.Tag
      |> Ash.Changeset.for_create(:seed, attrs, authorize?: false)
      |> Ash.create!(authorize?: false)
    end

    defp applied_tag(item_tag), do: Ash.load!(item_tag, :tag, authorize?: false).tag

    test "is the tag citing the identifier, whatever it is called", %{
      bout: bout,
      moderator: moderator
    } do
      j = tag!(%{name: "J", kind: :animal, iri: "SSA:0000020"})

      {:ok, item_tag} =
        apply_tag(bout, moderator, %{
          tag: %{name: "J pod", kind: "animal", iri: "SSA:0000020"}
        })

      tag = applied_tag(item_tag)
      assert tag.id == j.id
      assert tag.name == "J"
    end

    test "is the tag with that name, which gains the identifier it lacked", %{
      bout: bout,
      moderator: moderator
    } do
      bird = tag!(%{name: "bird"})

      {:ok, item_tag} =
        apply_tag(bout, moderator, %{tag: %{name: "Bird", kind: "animal", iri: "SSA:0000907"}})

      tag = applied_tag(item_tag)
      assert tag.id == bird.id
      assert {tag.kind, tag.iri} == {:animal, "SSA:0000907"}
    end

    test "is refused when the name is a tag of another kind", %{
      bout: bout,
      moderator: moderator
    } do
      tag!(%{name: "call", kind: :signal})

      assert {:error, %Ash.Error.Invalid{}} =
               apply_tag(bout, moderator, %{tag: %{name: "call", kind: "other"}})
    end

    test "given by id, is that tag, and nothing sent beside the id rewrites it", %{
      bout: bout,
      moderator: moderator
    } do
      k = tag!(%{name: "K", kind: :animal, iri: "SSA:0000021"})

      {:ok, item_tag} =
        apply_tag(bout, moderator, %{tag: %{id: k.id, name: "K pod", description: "renamed?"}})

      tag = applied_tag(item_tag)
      assert tag.id == k.id
      assert {tag.name, tag.description, tag.iri} == {"K", nil, "SSA:0000021"}
    end

    test "given by id, cannot be given another kind or identifier", %{
      bout: bout,
      moderator: moderator
    } do
      k = tag!(%{name: "K", kind: :animal, iri: "SSA:0000021"})

      assert {:error, %Ash.Error.Invalid{}} =
               apply_tag(bout, moderator, %{tag: %{id: k.id, name: "K", kind: "other"}})

      assert {:error, %Ash.Error.Invalid{}} =
               apply_tag(bout, moderator, %{tag: %{id: k.id, name: "K", iri: "SSA:0000099"}})
    end

    test "is the tag a new name's slug would collide with, which keeps its name", %{
      bout: bout,
      moderator: moderator
    } do
      biggs = tag!(%{name: "Bigg's", kind: :animal, iri: "SSA:0000002"})

      {:ok, item_tag} = apply_tag(bout, moderator, %{tag: %{name: "Biggs"}})

      tag = applied_tag(item_tag)
      assert tag.id == biggs.id
      assert tag.name == "Bigg's"
    end

    test "is refused when the name cites a different identifier", %{
      bout: bout,
      moderator: moderator
    } do
      tag!(%{name: "Gull", kind: :animal, iri: "SSA:0000908"})

      assert {:error, %Ash.Error.Invalid{}} =
               apply_tag(bout, moderator, %{tag: %{name: "Gull", iri: "SSA:0010097"}})
    end

    test "is refused when its id and identifier name different tags", %{
      bout: bout,
      moderator: moderator
    } do
      tag!(%{name: "J", kind: :animal, iri: "SSA:0000020"})
      k = tag!(%{name: "K", kind: :animal, iri: "SSA:0000021"})

      assert {:error, %Ash.Error.Invalid{}} =
               apply_tag(bout, moderator, %{
                 tag: %{id: k.id, name: "K", iri: "SSA:0000020"}
               })
    end

    test "is a new tag, with the kind and identifier the picker sent", %{
      bout: bout,
      moderator: moderator
    } do
      {:ok, item_tag} =
        apply_tag(bout, moderator, %{
          tag: %{name: "Pigeon guillemot", kind: "animal", iri: "SSA:0000909"}
        })

      tag = applied_tag(item_tag)
      assert {tag.name, tag.kind, tag.iri} == {"Pigeon guillemot", :animal, "SSA:0000909"}
    end

    test "free text still makes a tag nobody has classified", %{bout: bout, moderator: moderator} do
      {:ok, item_tag} = apply_tag(bout, moderator, %{tag: %{name: "mystery whup"}})

      tag = applied_tag(item_tag)
      assert {tag.kind, tag.iri} == {nil, nil}
    end
  end

  describe "set_certainty" do
    test "can say nothing again", %{bout: bout, moderator: moderator} do
      {:ok, item_tag} = apply_tag(bout, moderator, %{certainty: :certain})

      assert %ItemTag{certainty: nil} =
               item_tag
               |> Ash.Changeset.for_update(:set_certainty, %{certainty: nil}, actor: moderator)
               |> Ash.update!()
    end

    test "steps a moderator's own application", %{bout: bout, moderator: moderator} do
      {:ok, item_tag} = apply_tag(bout, moderator, %{certainty: :certain})

      assert %ItemTag{certainty: :probable} =
               item_tag
               |> Ash.Changeset.for_update(:set_certainty, %{certainty: :probable},
                 actor: moderator
               )
               |> Ash.update!()
    end

    test "is not for another moderator's application", %{bout: bout, moderator: moderator} do
      {:ok, item_tag} = apply_tag(bout, moderator, %{certainty: :certain})
      other = Orcasite.Generators.Accounts.create_user!(moderator: true)

      assert {:error, %Ash.Error.Forbidden{}} =
               item_tag
               |> Ash.Changeset.for_update(:set_certainty, %{certainty: :possible}, actor: other)
               |> Ash.update()
    end
  end
end
