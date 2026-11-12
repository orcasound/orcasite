defmodule OrcasiteWeb.AudioImageSubscriptionTest do
  @moduledoc """
  The bout page learns of each spectrogram it asked for through the
  audioImageUpdated subscription. The images are made in bulk by a background
  job, so this checks that bulk creation still reaches a subscriber.
  """

  # The subscription's results are read in another process, so the sandbox
  # has to be shared.
  use OrcasiteWeb.ChannelCase, async: false
  use Absinthe.Phoenix.SubscriptionTest, schema: OrcasiteWeb.Schema
  use Oban.Testing, repo: Orcasite.Repo

  import Orcasite.Generators.Radio

  alias Orcasite.Radio.FeedSegment
  alias Orcasite.Radio.Workers.GenerateFeedSpectrograms

  @subscription """
  subscription audioImageUpdated($feedId: String!, $startTime: DateTime!, $endTime: DateTime!) {
    audioImageUpdated(feedId: $feedId, startTime: $startTime, endTime: $endTime) {
      created { id objectPath }
    }
  }
  """

  test "a subscriber hears of each image the job makes" do
    feed = create_feed!()
    start_time = DateTime.add(DateTime.utc_now(), -1, :hour)
    end_time = DateTime.add(start_time, 10, :second)

    Ash.Seed.seed!(%FeedSegment{
      feed_id: feed.id,
      start_time: start_time,
      end_time: end_time,
      duration: Decimal.new(10),
      bucket: feed.bucket,
      bucket_region: "us-west-2",
      segment_path: "/#{feed.node_name}/hls/1700000000/live0.ts",
      file_name: "live0.ts"
    })

    {:ok, socket} = Phoenix.ChannelTest.connect(OrcasiteWeb.UserSocket, %{})
    {:ok, socket} = Absinthe.Phoenix.SubscriptionTest.join_absinthe(socket)

    ref =
      push_doc(socket, @subscription,
        variables: %{
          "feedId" => feed.id,
          "startTime" => DateTime.to_iso8601(start_time),
          "endTime" => DateTime.to_iso8601(end_time)
        }
      )

    assert_reply ref, :ok, %{subscriptionId: subscription_id}

    Oban.Testing.with_testing_mode(:manual, fn ->
      :ok =
        perform_job(GenerateFeedSpectrograms, %{
          feed_id: feed.id,
          start_time: DateTime.to_iso8601(start_time),
          end_time: DateTime.to_iso8601(end_time)
        })
    end)

    assert_push "subscription:data",
                %{
                  subscriptionId: ^subscription_id,
                  result: %{
                    data: %{"audioImageUpdated" => %{"created" => %{"objectPath" => path}}}
                  }
                },
                5_000

    assert String.ends_with?(path, ".webp")
  end
end
