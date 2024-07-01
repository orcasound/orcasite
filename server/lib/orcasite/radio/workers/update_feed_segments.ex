defmodule Orcasite.Radio.Workers.UpdateFeedSegments do
  use Oban.Worker,
    queue: :feeds,
    unique: [keys: [:feed_stream_id], period: :infinity, states: [:available, :scheduled, :executing]]

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"feed_stream_id" => feed_stream_id}}) do
    Orcasite.Radio.FeedStream
    |> Orcasite.Radio.get!(feed_stream_id)
    |> Ash.Changeset.for_update(:update_segments)
    |> Orcasite.Radio.update()
  end
end
