defmodule Orcasite.Radio.Bout.Changes.SeedTags do
  @moduledoc """
  Attach production's tags to a seeded bout (Bout.seed's `tags` argument).

  Each tag is upserted as production has it, id included (Tag.seed), and joined to the
  bout through one item_tags row with no user (ItemTag.seed). Done by hand rather than
  with `manage_relationship`: a seed is an upsert, which manages the relationship as if
  the bout were new, so every re-seed -- and review apps re-seed every minute -- would add
  another join row. This creates the join only when none exists yet.
  """

  use Ash.Resource.Change

  require Ash.Query

  alias Orcasite.Radio.{ItemTag, Tag}

  @impl true
  def change(changeset, _opts, _context) do
    tags = Ash.Changeset.get_argument(changeset, :tags) || []

    Ash.Changeset.after_action(changeset, fn _changeset, bout ->
      Enum.each(tags, fn attrs ->
        tag = Ash.create!(Tag, attrs, action: :seed, authorize?: false)

        joined? =
          ItemTag
          |> Ash.Query.filter(bout_id == ^bout.id and tag_id == ^tag.id and is_nil(user_id))
          |> Ash.exists?(authorize?: false)

        unless joined? do
          Ash.create!(ItemTag, %{bout_id: bout.id, tag_id: tag.id},
            action: :seed,
            authorize?: false
          )
        end
      end)

      {:ok, bout}
    end)
  end
end
