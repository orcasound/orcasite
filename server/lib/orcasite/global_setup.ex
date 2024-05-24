defmodule Orcasite.GlobalSetup do
  def populate_feed_streams do
    Orcasite.Radio.Feed
    |> Ash.Query.for_read(:read)
    |> Orcasite.Radio.read!()
    |> Enum.map(fn feed ->
      with {:ok, %{timestamps: timestamps}} <- Orcasite.Radio.AwsClient.list_timestamps(feed) do
        timestamps
        |> Enum.map(&%{feed: feed, playlist_timestamp: &1})
        |> Orcasite.Radio.bulk_create(Orcasite.Radio.FeedStream, :create)
      end
    end)
  end
end
