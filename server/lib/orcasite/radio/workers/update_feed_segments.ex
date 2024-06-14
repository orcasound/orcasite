defmodule Orcasite.Radio.Workers.UpdateFeedSegments do
  use Oban.Worker, queue: :feed_segments, unique: [keys: [:feed_stream_id], period: 10]

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"feed_stream_id" => feed_stream_id}}) do
    Orcasite.Radio.FeedStream
    |> Orcasite.Radio.get!(feed_stream_id)
    |> Ash.Changeset.for_update(:update_segments)
    |> Orcasite.Radio.update()
  end
end
