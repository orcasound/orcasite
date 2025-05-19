defmodule Orcasite.Radio.Seed.SeedFeedStreams do
  use Ash.Resource.Change

  alias Orcasite.Radio.FeedStream
  alias Orcasite.Radio.Seed.Utils

  @impl true
  def change(changeset, _opts, _context) do
    resource = FeedStream

    changeset
    |> Ash.Changeset.before_action(fn change ->
      local_feed_id = change |> Ash.Changeset.get_argument(:feed_id)
      prod_feed_id = Utils.local_to_prod_feed_id(local_feed_id)

      from_date = change |> Ash.Changeset.get_argument(:start_time)
      to_date = change |> Ash.Changeset.get_argument(:end_time)

      feed_streams =
        Orcasite.Radio.GraphqlClient.feed_streams(prod_feed_id, from_date, to_date)
        |> Utils.prepare_results(resource)

      count = Enum.count(feed_streams)

      feed_streams
      |> Ash.bulk_create(resource, :create,
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
end
