defmodule Orcasite.Radio.AudioImageTest do
  @moduledoc """
  Asking for spectrograms over a range of a feed makes an image per segment.
  The request only queues that work, because a range of a couple of hours used
  to outlast the request's database connection and was rolled back whole.

  The job upserts, so a range the archive already covers must leave every
  existing image pointing at the object it has, even though new images are
  named for a different format than the archive was rendered in.
  """

  use Orcasite.DataCase, async: true
  use Oban.Testing, repo: Orcasite.Repo

  import Orcasite.Generators.Radio

  require Ash.Query
  import Ecto.Query, only: [from: 2]

  alias Orcasite.Radio.{AudioImage, AudioImageFeedSegment, FeedSegment}
  alias Orcasite.Radio.Workers.{GenerateFeedSpectrograms, GenerateSpectrogram}

  setup do
    feed = create_feed!()
    start_time = DateTime.add(DateTime.utc_now(), -1, :hour)

    %{feed: feed, start_time: start_time}
  end

  defp seed_segments!(feed, start_time, count) do
    for i <- 0..(count - 1) do
      segment_start = DateTime.add(start_time, 10 * i, :second)

      Ash.Seed.seed!(%FeedSegment{
        feed_id: feed.id,
        start_time: segment_start,
        end_time: DateTime.add(segment_start, 10, :second),
        duration: Decimal.new(10),
        bucket: feed.bucket,
        bucket_region: "us-west-2",
        segment_path: "/#{feed.node_name}/hls/1700000000/live#{i}.ts",
        file_name: "live#{i}.ts"
      })
    end
  end

  # Rendering would reach for AWS, so leave every job queued and run only the
  # one that makes the images.
  defp request_spectrograms(feed, start_time, end_time) do
    Oban.Testing.with_testing_mode(:manual, fn ->
      :ok =
        perform_job(GenerateFeedSpectrograms, %{
          feed_id: feed.id,
          start_time: DateTime.to_iso8601(start_time),
          end_time: DateTime.to_iso8601(end_time)
        })
    end)
  end

  defp images(feed) do
    AudioImage
    |> Ash.Query.filter(feed_id == ^feed.id)
    |> Ash.Query.load(:feed_segments)
    |> Ash.read!(authorize?: false)
  end

  test "asking for spectrograms queues the work and returns", %{feed: feed} = context do
    end_time = DateTime.add(context.start_time, 1, :hour)
    seed_segments!(feed, context.start_time, 3)

    Oban.Testing.with_testing_mode(:manual, fn ->
      feed
      |> Ash.Changeset.for_update(:generate_spectrogram, %{
        start_time: context.start_time,
        end_time: end_time
      })
      |> Ash.update!(authorize?: false)

      assert_enqueued(
        worker: GenerateFeedSpectrograms,
        args: %{
          feed_id: feed.id,
          start_time: DateTime.to_iso8601(context.start_time),
          end_time: DateTime.to_iso8601(end_time)
        }
      )
    end)

    assert images(feed) == []
  end

  test "a new image is named for the format the renderer is asked for, linked and queued",
       %{feed: feed, start_time: start_time} do
    [segment] = seed_segments!(feed, start_time, 1)
    request_spectrograms(feed, start_time, segment.end_time)

    assert [image] = images(feed)
    assert image.start_time == segment.start_time
    assert image.end_time == segment.end_time
    assert image.image_type == :spectrogram
    assert String.starts_with?(image.object_path, "/#{feed.node_name}/spectrograms/")
    assert String.ends_with?(image.object_path, "__#{image.id}.webp")
    assert Enum.map(image.feed_segments, & &1.id) == [segment.id]
    assert_enqueued(worker: GenerateSpectrogram, args: %{audio_image_id: image.id})
  end

  test "an existing image keeps the object it has", %{feed: feed, start_time: start_time} do
    [segment] = seed_segments!(feed, start_time, 1)

    existing =
      Ash.Seed.seed!(%AudioImage{
        feed_id: feed.id,
        image_type: :spectrogram,
        status: :complete,
        start_time: segment.start_time,
        end_time: segment.end_time,
        bucket: "test-audio-deriv-orcasound-net",
        bucket_region: "us-west-2",
        object_path: "/#{feed.node_name}/spectrograms/2026-01-01/archive.png"
      })

    Ash.Seed.seed!(%AudioImageFeedSegment{
      audio_image_id: existing.id,
      feed_segment_id: segment.id
    })

    request_spectrograms(feed, start_time, segment.end_time)

    assert [image] = images(feed)
    assert image.id == existing.id
    assert image.object_path == existing.object_path
    assert Enum.map(image.feed_segments, & &1.id) == [segment.id]
    refute_enqueued(worker: GenerateSpectrogram)
  end

  test "an image whose render is already queued isn't queued again",
       %{feed: feed, start_time: start_time} do
    [segment] = seed_segments!(feed, start_time, 1)
    request_spectrograms(feed, start_time, segment.end_time)
    request_spectrograms(feed, start_time, segment.end_time)

    assert [image] = images(feed)
    assert [_] = all_enqueued(worker: GenerateSpectrogram, args: %{audio_image_id: image.id})
  end

  test "an image whose render is waiting to retry isn't queued again",
       %{feed: feed, start_time: start_time} do
    [segment] = seed_segments!(feed, start_time, 1)
    request_spectrograms(feed, start_time, segment.end_time)
    [image] = images(feed)

    worker = Oban.Worker.to_string(GenerateSpectrogram)

    {1, _} =
      Orcasite.Repo.update_all(from(job in Oban.Job, where: job.worker == ^worker),
        set: [state: "retryable"]
      )

    request_spectrograms(feed, start_time, segment.end_time)

    assert [] = all_enqueued(worker: GenerateSpectrogram, args: %{audio_image_id: image.id})
  end

  test "an existing image with no object path is given one before it's rendered",
       %{feed: feed, start_time: start_time} do
    [segment] = seed_segments!(feed, start_time, 1)

    existing =
      Ash.Seed.seed!(%AudioImage{
        feed_id: feed.id,
        image_type: :spectrogram,
        status: :new,
        start_time: segment.start_time,
        end_time: segment.end_time,
        bucket: "test-audio-deriv-orcasound-net",
        bucket_region: "us-west-2"
      })

    request_spectrograms(feed, start_time, segment.end_time)

    assert [image] = images(feed)
    assert image.id == existing.id
    assert String.ends_with?(image.object_path, "__#{image.id}.webp")
    assert_enqueued(worker: GenerateSpectrogram, args: %{audio_image_id: image.id})
  end

  test "asking again after the range's job has finished queues it again",
       %{feed: feed} = context do
    end_time = DateTime.add(context.start_time, 1, :hour)

    ask = fn ->
      feed
      |> Ash.Changeset.for_update(:generate_spectrogram, %{
        start_time: context.start_time,
        end_time: end_time
      })
      |> Ash.update!(authorize?: false)
    end

    Oban.Testing.with_testing_mode(:manual, fn ->
      ask.()
      worker = Oban.Worker.to_string(GenerateFeedSpectrograms)

      {1, _} =
        Orcasite.Repo.update_all(from(job in Oban.Job, where: job.worker == ^worker),
          set: [state: "completed"]
        )

      ask.()
      assert [_] = all_enqueued(worker: GenerateFeedSpectrograms)
    end)
  end

  test "a range longer than one batch gets an image for every segment",
       %{feed: feed, start_time: start_time} do
    segments = seed_segments!(feed, start_time, 450)
    request_spectrograms(feed, start_time, List.last(segments).end_time)

    images = images(feed)
    assert length(images) == 450

    assert images |> Enum.flat_map(& &1.feed_segments) |> Enum.map(& &1.id) |> Enum.sort() ==
             segments |> Enum.map(& &1.id) |> Enum.sort()

    assert length(all_enqueued(worker: GenerateSpectrogram)) == 450
  end
end
