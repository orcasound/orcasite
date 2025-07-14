defmodule Orcasite.Radio.Feed.Changes.StoreLatestListenerCount do
  use Ash.Resource.Change

  @impl Ash.Resource.Change
  def change(changeset, _opts, _context) do
    changeset
    |> Ash.Changeset.before_action(fn change ->
      feed_slug = change.data.slug

      Orcasite.Cache.get("listener_counts:" <> feed_slug)
      |> case do
        nil ->
          change

        0 ->
          change

        count ->
          change
          |> Ash.Changeset.manage_relationship(
            :listener_counts,
            [
              %{
                count: count,
                timestamp: DateTime.utc_now()
              }
            ],
            type: :create
          )
      end
    end)
  end
end
