defmodule Orcasite.Radio.ItemTag.Changes.ResolveTag do
  @moduledoc """
  Which tag a bout tag applies, when the picker sends a name, and perhaps the register
  identifier it means, rather than a tag id (#1015).

  A tag that cites the identifier is that tag, whatever it is called: the "J pod" button
  applies production's `J`, which cites `SSA:0000020`. Otherwise the tag with the same
  name, compared case-insensitively as the name index compares them; when the picker
  knows a kind or identifier that tag lacks, the tag gets it, filling a null as the
  classification migration does and never overwriting. Otherwise a new tag, with the
  kind and identifier the picker sent.

  A name already taken by a tag citing a different identifier, or of a different kind,
  is refused rather than guessed at: the two vocabularies disagree, and a moderator
  should see it.
  """

  use Ash.Resource.Change

  require Ash.Query

  alias Orcasite.Radio.Tag

  @impl true
  def change(changeset, _opts, _context) do
    case Ash.Changeset.get_argument(changeset, :tag) do
      %{} = tag ->
        case resolve(tag) do
          {:ok, resolved} ->
            Ash.Changeset.set_argument(changeset, :tag, resolved)

          {:error, message} ->
            Ash.Changeset.add_error(changeset, field: :tag, message: message)
        end

      _ ->
        changeset
    end
  end

  defp resolve(tag) do
    id = blank_to_nil(tag[:id])
    iri = blank_to_nil(tag[:iri])
    kind = blank_to_nil(tag[:kind])
    cited = iri && read_one(Ash.Query.filter(Tag, iri == ^iri))

    cond do
      # a tag id names the tag outright, unless an identifier beside it names another
      id && cited && cited.id != id ->
        {:error, "the tag given by id is not the one citing #{iri}"}

      id ->
        case read_one(Ash.Query.filter(Tag, id == ^id)) do
          nil -> {:error, "there is no tag #{id}"}
          existing -> reconcile(existing, tag[:name], iri, kind)
        end

      cited ->
        reconcile(cited, tag[:name], iri, kind)

      existing = by_name(tag[:name]) ->
        reconcile(existing, tag[:name], iri, kind)

      true ->
        {:ok, tag |> Map.put(:iri, iri) |> Map.put(:kind, kind)}
    end
  end

  # Only ever fills what an existing tag lacks. The map given to the relationship is the
  # tag's own name and id plus those fills, so relating to it updates nothing else: the
  # update it runs accepts every attribute, and would take a name or kind it was handed.
  defp reconcile(existing, name, iri, kind) do
    cond do
      existing.iri && iri && existing.iri != iri ->
        {:error, "#{name} is already a tag citing #{existing.iri}, not #{iri}"}

      existing.kind && kind && to_string(existing.kind) != to_string(kind) ->
        {:error, "#{name} is already a tag for #{existing.kind}, not #{kind}"}

      true ->
        {:ok,
         %{id: existing.id, name: existing.name}
         |> fill(:iri, existing.iri, iri)
         |> fill(:kind, existing.kind, kind)}
    end
  end

  # The same name as the unique index compares names (ignoring case), or the same slug,
  # which Tag.create upserts on: `Biggs` is a new name but `Bigg's`'s slug, and creating
  # it would rename production's tag rather than make another.
  defp by_name(name) when is_binary(name) do
    lowered = String.downcase(name)
    slug = Slug.slugify(name)

    read_one(Ash.Query.filter(Tag, fragment("lower(?)", name) == ^lowered)) ||
      (slug && read_one(Ash.Query.filter(Tag, slug == ^slug)))
  end

  defp by_name(_), do: nil

  defp read_one(query), do: Ash.read_one!(query, authorize?: false)

  defp fill(map, key, nil, value) when not is_nil(value), do: Map.put(map, key, value)
  defp fill(map, _key, _existing, _value), do: map

  defp blank_to_nil(""), do: nil
  defp blank_to_nil(value), do: value
end
