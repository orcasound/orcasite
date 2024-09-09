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
    |> Orcasite.Radio.AwsClient.request_timestamps()
    |> case do
      {:ok, %{timestamps: [timestamp | _]}} ->
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
end
