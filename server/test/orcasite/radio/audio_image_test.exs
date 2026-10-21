defmodule Orcasite.Radio.AudioImageTest do
  @moduledoc """
  Asking for spectrograms over a range upserts an image per segment, so a
  range the archive already covers must leave every existing image pointing
  at the object it has, even though new images are named for a different
  format than the archive was rendered in.
  """

  use Orcasite.DataCase, async: true

  import Orcasite.Generators.Radio

  alias Orcasite.Radio.{AudioImage, AudioImageFeedSegment, FeedSegment}

  setup do
    feed = create_feed!()
    start_time = DateTime.add(DateTime.utc_now(), -1, :hour)
    end_time = DateTime.add(start_time, 10, :second)

    segment =
      Ash.Seed.seed!(%FeedSegment{
        feed_id: feed.id,
        start_time: start_time,
        end_time: end_time,
        duration: Decimal.new(10),
        bucket: feed.bucket,
        bucket_region: "us-west-2",
        segment_path: "/#{feed.node_name}/hls/1700000000/live000.ts",
        file_name: "live000.ts"
      })

    %{feed: feed, segment: segment, start_time: start_time, end_time: end_time}
  end

  # Creating an image enqueues its render, which the test suite runs inline
  # and which would then reach for AWS; leave the job queued instead.
  defp request_spectrogram(segment) do
    Oban.Testing.with_testing_mode(:manual, fn ->
      AudioImage
      |> Ash.Changeset.for_create(:for_feed_segment, %{feed_segment_id: segment.id})
      |> Ash.create!(authorize?: false)
    end)
  end

  test "a new image is named for the format the renderer is asked for", %{segment: segment} do
    image = request_spectrogram(segment)
    assert String.ends_with?(image.object_path, ".webp")
    assert String.starts_with?(image.object_path, "/")
  end

  test "an existing image keeps the object it has", %{feed: feed, segment: segment} = context do
    existing =
      Ash.Seed.seed!(%AudioImage{
        feed_id: feed.id,
        image_type: :spectrogram,
        status: :complete,
        start_time: context.start_time,
        end_time: context.end_time,
        bucket: "test-audio-deriv-orcasound-net",
        bucket_region: "us-west-2",
        object_path: "/#{feed.node_name}/spectrograms/2026-01-01/archive.png"
      })

    Ash.Seed.seed!(%AudioImageFeedSegment{
      audio_image_id: existing.id,
      feed_segment_id: segment.id
    })

    image = request_spectrogram(segment)
    assert image.id == existing.id
    assert image.object_path == existing.object_path
  end
end
