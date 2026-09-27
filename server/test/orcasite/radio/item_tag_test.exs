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
end
