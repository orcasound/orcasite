defmodule Orcasite.Radio.Workers.LinkFeedStream do
  use Oban.Worker, queue: :feeds, unique: [keys: [:feed_stream_id], period: 10]

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"feed_stream_id" => feed_stream_id} = args}) do
    enqueue_prev_stream = Map.get(args, "enqueue_prev_stream", false)
    enqueue_next_stream = Map.get(args, "enqueue_next_stream", false)
    next_depth = Map.get(args, "next_depth", 3)
    prev_depth = Map.get(args, "prev_depth", 3)

    feed_stream =
      Orcasite.Radio.FeedStream
      |> Orcasite.Radio.get!(feed_stream_id)

    # If new link to next stream, update times and maybe queue next feed_stream to link
    feed_stream
    |> Ash.Changeset.for_update(:link_next_stream)
    |> Orcasite.Radio.update()
    |> case do
      {:ok, %{next_feed_stream_id: next_feed_stream_id} = fs} ->
        fs
        |> Ash.Changeset.for_update(:update_end_time_and_duration)
        |> Orcasite.Radio.update()

        if enqueue_next_stream and next_depth > 0 do
          %{
            feed_stream_id: next_feed_stream_id,
            enqueue_next_stream: enqueue_next_stream,
            next_depth: next_depth - 1
          }
          |> new()
          |> Oban.insert()
        end

      _ ->
        nil
    end

    feed_stream
    |> Ash.Changeset.for_update(:link_prev_stream)
    |> Orcasite.Radio.update()
    |> case do
      {:ok, %{prev_feed_stream_id: prev_feed_stream_id}} ->
        # If new link to previous stream, queue another link job
        if enqueue_prev_stream and prev_depth > 0 do
          %{
            feed_stream_id: prev_feed_stream_id,
            enqueue_prev_stream: enqueue_prev_stream,
            prev_depth: prev_depth - 1
          }
          |> new()
          |> Oban.insert()
        end

      _ ->
        nil
    end

    :ok
  end
end