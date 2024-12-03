defmodule Orcasite.GlobalSetup do
  def populate_feed_streams do
    Orcasite.Radio.Feed
    |> Ash.Query.for_read(:read)
    |> Ash.read!()
    |> Stream.map(fn feed ->
      Orcasite.Radio.AwsClient.list_timestamps(feed, fn timestamps ->
        timestamps
        |> Enum.map(&%{feed: feed, playlist_timestamp: &1})
        |> Ash.bulk_create(Orcasite.Radio.FeedStream, :create)
      end)

      :ok
    end)
    |> Enum.to_list()
  end

  def populate_latest_feed_segments(feed) do
    feed
    |> Orcasite.Radio.AwsClient.list_timestamps()
    |> case do
      {:ok, %{timestamps: [_ | _] = timestamps}} ->
        timestamp = List.last(timestamps)

        {:ok, feed_stream} =
          Orcasite.Radio.FeedStream
          |> Ash.Changeset.for_create(:create, %{feed: feed, playlist_timestamp: timestamp})
          |> Ash.create(authorize?: false)

        feed_stream
        |> Ash.Changeset.for_update(:update_segments)
        |> Ash.update(authorize?: false)

      _ ->
        :ok
    end
  end

  def populate_latest_feed_streams(feed, minutes_ago \\ 10) do
    if Application.get_env(:orcasite, :env) != :prod do
      # Get prod feed id for feed
      {:ok, feed_resp} = Orcasite.Radio.GraphqlClient.get_feed(feed.slug)
      feed_id = feed_resp |> get_in(["data", "feed", "id"])

      now = DateTime.utc_now()
      minutes_ago_datetime = now |> DateTime.add(-minutes_ago, :minute)

      # Get stream for the last `minutes` minutes
      {:ok, feed_streams_response} =
        Orcasite.Radio.GraphqlClient.get_feed_streams_with_segments(feed_id, minutes_ago_datetime, now)

      feed_streams = get_in(feed_streams_response, ["data", "feedStreams", "results"])

      feed_streams
      |> Enum.map(
        &%{
          m3u8_path: Map.get(&1, "playlistM3u8Path"),
          bucket: Map.get(&1, "bucket"),
          update_segments?: true,
          link_streams?: true
        }
      )
      |> Ash.bulk_create(
        Orcasite.Radio.FeedStream,
        :from_m3u8_path,
        return_errors?: true,
        stop_on_error?: true,
        upsert?: true,
        upsert_identity: :feed_stream_timestamp
      )
    end
  end
end
