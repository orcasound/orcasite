defmodule Orcasite.Radio.Workers.GenerateFeedSpectrograms do
  @moduledoc """
  Makes a spectrogram image for every feed segment in a range of a feed's
  audio, and queues each one that isn't rendered yet.

  A moderator asks for this from the bout page, over whatever range the page
  shows, which can be a couple of hours or about 700 segments. Images are made
  a batch at a time: one insert for the images, one for their links to
  segments and one for their render jobs, rather than several queries each.
  """

  use Oban.Worker,
    queue: :audio_images,
    max_attempts: 3,
    # The button is pressed again when nothing seems to happen.
    unique: [period: 60, keys: [:feed_id, :start_time, :end_time]]

  import Ecto.Query, only: [from: 2]

  alias Orcasite.Radio.{AudioImage, AudioImageFeedSegment, Feed, FeedSegment}
  alias Orcasite.Radio.Workers.GenerateSpectrogram

  @batch_size 200

  @impl Oban.Worker
  def perform(%Oban.Job{
        args: %{"feed_id" => feed_id, "start_time" => start_time, "end_time" => end_time}
      }) do
    feed = Ash.get!(Feed, feed_id, authorize?: false)

    %{feed_id: feed.id, start_time: parse_time(start_time), end_time: parse_time(end_time)}
    |> FeedSegment.for_feed_range!(authorize?: false)
    # Segments that cover the same span share one image.
    |> Enum.group_by(&span/1)
    |> Enum.chunk_every(@batch_size)
    |> Enum.each(&make_images(&1, feed))
  end

  @impl Oban.Worker
  def timeout(_job), do: :timer.minutes(5)

  defp make_images(segments_by_span, feed) do
    %Ash.BulkResult{status: :success, records: images} =
      segments_by_span
      |> Enum.map(fn {_span, [segment | _]} ->
        %{
          feed_id: feed.id,
          start_time: segment.start_time,
          end_time: segment.end_time,
          node_name: feed.node_name
        }
      end)
      |> Ash.bulk_create(AudioImage, :for_span,
        authorize?: false,
        return_records?: true,
        return_errors?: true,
        stop_on_error?: true,
        notify?: true
      )

    segments_by_span = Map.new(segments_by_span)

    %Ash.BulkResult{status: :success} =
      for image <- images, segment <- Map.fetch!(segments_by_span, span(image)) do
        %{audio_image_id: image.id, feed_segment_id: segment.id}
      end
      |> Ash.bulk_create(AudioImageFeedSegment, :create,
        authorize?: false,
        # An existing link is left as it is.
        upsert_fields: [:updated_at],
        return_errors?: true,
        stop_on_error?: true
      )

    unrendered = images |> Enum.reject(&(&1.status == :complete)) |> Enum.map(& &1.id)
    queued = queued_renders(unrendered)

    unrendered
    |> Enum.reject(&MapSet.member?(queued, &1))
    |> Enum.map(&GenerateSpectrogram.new(%{audio_image_id: &1}))
    |> Oban.insert_all()
  end

  # Oban's basic engine ignores a worker's uniqueness in insert_all, so leave
  # out the images whose render is already waiting or running, as inserting
  # them one at a time would have.
  defp queued_renders(image_ids) do
    worker = Oban.Worker.to_string(GenerateSpectrogram)

    from(job in Oban.Job,
      where: job.worker == ^worker,
      where: job.state in ["available", "scheduled", "executing"],
      where: fragment("? ->> 'audio_image_id'", job.args) in ^image_ids,
      select: fragment("? ->> 'audio_image_id'", job.args)
    )
    |> Orcasite.Repo.all()
    |> MapSet.new()
  end

  defp span(%{start_time: start_time, end_time: end_time}),
    do: {DateTime.to_unix(start_time, :microsecond), DateTime.to_unix(end_time, :microsecond)}

  defp parse_time(iso8601) do
    {:ok, time, _offset} = DateTime.from_iso8601(iso8601)
    time
  end
end
