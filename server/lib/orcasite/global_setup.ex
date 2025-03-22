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

  def populate_latest_feed_segments_from_aws(feed) do
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

  def populate_latest_feed_streams_from_prod(feed, minutes_ago \\ 10) do
    now = DateTime.utc_now()
    from_time = now |> DateTime.add(-minutes_ago, :minute)
    populate_feed_streams_range(feed, from_time, now)
  end

  def populate_feed_streams_range(feed, from_time, to_time) do
    if Application.get_env(:orcasite, :env) != :prod do
      # Get prod feed id for feed
      {:ok, feed_resp} = Orcasite.Radio.GraphqlClient.get_feed(feed.slug)

      feed_resp
      |> get_in(["data", "feed", "id"])
      |> case do
        nil ->
          {:error, :feed_not_found}

        feed_id ->
          # Get stream in time range (from_time, to_time)
          {:ok, feed_streams_response} =
            Orcasite.Radio.GraphqlClient.get_feed_streams_with_segments(
              feed_id,
              from_time,
              to_time
            )

          feed_streams = get_in(feed_streams_response, ["data", "feedStreams", "results"])

          feed_streams
          |> Recase.Enumerable.convert_keys(&Recase.to_snake/1)
          |> Enum.map(fn feed_stream ->
            feed_stream
            |> Map.drop(["id"])
            |> Map.put("feed", feed)
            |> Map.update(
              "feed_segments",
              [],
              &Enum.flat_map(&1, fn seg ->
                seg =
                  seg
                  |> Map.drop(["id"])
                  |> Map.put("feed", feed)
                  |> Recase.Enumerable.atomize_keys()

                # Only include segments in time range
                {:ok, start_time, _} = seg.start_time |> DateTime.from_iso8601()
                {:ok, end_time, _} = seg.end_time |> DateTime.from_iso8601()

                if times_overlap(start_time, end_time, from_time, to_time) do
                  [seg]
                else
                  []
                end
              end)
            )
            |> Recase.Enumerable.atomize_keys()
          end)
          |> Ash.bulk_create(
            Orcasite.Radio.FeedStream,
            :populate_with_segments,
            return_errors?: true,
            stop_on_error?: true,
            upsert?: true,
            upsert_identity: :playlist_m3u8_path
          )
      end
    end
  end

  defp times_overlap(start_1, end_1, start_2, end_2) do
    # Equivalent to start_1 <= end_2 && start_2 <= end_1
    DateTime.compare(end_2, start_1) != :lt and DateTime.compare(start_2, end_1) != :gt
  end
end
