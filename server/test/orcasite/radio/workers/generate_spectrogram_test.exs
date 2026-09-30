defmodule Orcasite.Radio.Workers.GenerateSpectrogramTest do
  @moduledoc """
  The bout page draws a shimmering placeholder for every image that is `new`
  or `processing`. Once Oban gives up on an image's job, the image must be
  `failed` so the placeholder resolves, whether the last attempt returned an
  error or raised.
  """

  use Orcasite.DataCase, async: true

  import Orcasite.Generators.Radio

  alias Orcasite.Radio.{AudioImage, AudioImageFeedSegment, FeedSegment}
  alias Orcasite.Radio.Workers.GenerateSpectrogram

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

    image =
      Ash.Seed.seed!(%AudioImage{
        feed_id: feed.id,
        image_type: :spectrogram,
        status: :new,
        start_time: start_time,
        end_time: end_time,
        bucket: "test-audio-deriv-orcasound-net",
        bucket_region: "us-west-2",
        object_path: "/#{feed.node_name}/spectrograms/#{image_name(start_time)}.png"
      })

    Ash.Seed.seed!(%AudioImageFeedSegment{audio_image_id: image.id, feed_segment_id: segment.id})

    %{image: image}
  end

  # There is no renderer to reach from the test suite, so every attempt
  # fails; what matters is what the failure leaves behind.
  defp attempt(image, attempt) do
    Oban.Testing.perform_job(GenerateSpectrogram, %{audio_image_id: image.id}, attempt: attempt)
  rescue
    exception -> {:raised, exception}
  end

  test "an attempt before the last leaves the image for the next one", %{image: image} do
    refute match?(:ok, attempt(image, 1))
    assert Ash.get!(AudioImage, image.id, authorize?: false).status in [:processing, :errored]
  end

  test "the last attempt marks the image failed", %{image: image} do
    refute match?(:ok, attempt(image, 3))
    assert Ash.get!(AudioImage, image.id, authorize?: false).status == :failed
  end

  defp image_name(time), do: time |> DateTime.truncate(:second) |> DateTime.to_iso8601(:basic)
end
