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
        tag = seed_tag(attrs)

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

  # Names are unique case-insensitively, so a tag someone already made here by hand
  # ("SRKW" with a local id) would collide with production's row of the same name. Reuse
  # it, giving it production's classification, and only create when there is no such tag;
  # then Tag.seed's upsert by id covers re-seeds.
  defp seed_tag(attrs) do
    name = attrs["name"] || attrs[:name]

    existing =
      Tag
      |> Ash.Query.filter(fragment("lower(?)", name) == ^String.downcase(name))
      |> Ash.read_one!(authorize?: false)

    case existing do
      nil ->
        Ash.create!(Tag, attrs, action: :seed, authorize?: false)

      tag ->
        updates =
          Map.new(["description", "kind", "iri"], fn key ->
            {String.to_atom(key), attrs[key] || attrs[String.to_atom(key)]}
          end)

        Ash.update!(tag, updates, authorize?: false)
    end
  end
end
