defmodule Orcasite.Radio.Seed.Changes.SeedFeeds do
  use Ash.Resource.Change

  alias Orcasite.Radio.Seed.Utils

  require Logger

  @impl true
  def change(changeset, _opts, _context) do
    changeset
    |> Ash.Changeset.before_action(fn change ->
      feeds =
        Orcasite.Radio.GraphqlClient.get_feeds()
        |> Utils.prepare_results(Orcasite.Radio.Feed)

      count = Enum.count(feeds)

      feeds
      |> Ash.bulk_create(Orcasite.Radio.Feed, :create, return_errors?: true, authorize?: false)
      |> case do
        %{status: :success} ->
          Logger.info("Successfully seeded #{count} feeds")

          change
          |> Ash.Changeset.force_change_attribute(:seeded_count, count)

        %{errors: errors} ->
          Logger.error("Error while seeding feeds: #{inspect(errors)}")

          change
          |> Ash.Changeset.add_error(errors)
      end
    end)
  end
end
