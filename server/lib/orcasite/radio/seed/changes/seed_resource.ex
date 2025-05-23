defmodule Orcasite.Radio.Seed.Changes.SeedResource do
  use Ash.Resource.Change

  alias Orcasite.Radio.Seed.Utils
  alias Orcasite.Radio.Seed.Types.Resource

  @impl true
  def change(changeset, _opts, _context) do
    resource_name = Ash.Changeset.get_argument(changeset, :resource)

    changeset
    |> Ash.Changeset.before_action(fn change ->
      feed_id = change |> Ash.Changeset.get_argument(:feed_id)

      from_date = change |> Ash.Changeset.get_argument(:start_time)
      to_date = change |> Ash.Changeset.get_argument(:end_time)

      bulk_resource = resource_name |> bulk_create_resource_name() |> Resource.to_module()
      bulk_action = resource_name |> bulk_create_action()

      inputs =
        Orcasite.Radio.GraphqlClient.get_resource(resource_name, feed_id, from_date, to_date)
        |> Utils.prepare_results(bulk_resource)

      count = Enum.count(inputs)

      inputs
      |> Ash.bulk_create(
        bulk_resource,
        bulk_action,
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
      name -> name
    end
  end

  def bulk_create_action(resource_name) do
    case resource_name do
      :feed_segment -> :populate_with_segments
      :candidate -> :seed
      :detection -> :seed
      :audio_image -> :seed
      _ -> :create
    end
  end
end
