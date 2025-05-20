defmodule Orcasite.Radio.Seed.SeedResource do
  use Ash.Resource.Change

  alias Orcasite.Radio.Seed.Utils
  alias Orcasite.Radio.Seed.Types.Resource

  @impl true
  def change(changeset, _opts, _context) do
    resource_name = Ash.Changeset.get_argument(changeset, :resource)
    resource = Resource.to_module(resource_name)

    changeset
    |> Ash.Changeset.before_action(fn change ->
      feed_id = change |> Ash.Changeset.get_argument(:feed_id)

      from_date = change |> Ash.Changeset.get_argument(:start_time)
      to_date = change |> Ash.Changeset.get_argument(:end_time)

      inputs =
        Orcasite.Radio.GraphqlClient.get_resource(resource_name, feed_id, from_date, to_date)
        |> Utils.prepare_results(resource)

      count = Enum.count(inputs)

      inputs
      |> Ash.bulk_create(
        resource |> bulk_create_resource_name() |> Resource.to_module(),
        resource |> bulk_create_action(),
        return_errors?: true,
        authorize?: false
      )
      |> case do
        %{status: :success} ->
          change
          |> Ash.Changeset.force_change_attribute(:seeded_count, count)

        %{errors: errors} ->
          change
          |> Ash.Changeset.add_error(errors)
      end
    end)
  end

  @doc """
  Resource to use when seeding data (e.g. for FeedSegment, the query returns feed_segments nested within
  feed_streams)
  """
  def bulk_create_resource_name(resource_name) do
    case resource_name do
      :feed_segment -> :feed_stream
      :detection -> :candidate
      name -> name
    end
  end

  def bulk_create_action(resource_name) do
    case resource_name do
      :feed_segment -> :populate_with_segments
      _ -> :creaet
    end
  end
end
