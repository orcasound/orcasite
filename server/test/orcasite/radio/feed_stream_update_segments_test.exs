defmodule Orcasite.Radio.FeedStreamUpdateSegmentsTest do
  @moduledoc """
  Runs the `:update_segments` action against the database, with the playlists
  a node would have on S3 supplied by the test (#1092).
  """

  use Orcasite.DataCase, async: true

  import ExUnit.CaptureLog
  import Orcasite.Generators.Radio

  require Ash.Query

  alias Orcasite.Radio.{FeedSegment, FeedStream}

  @start_time ~U[2026-09-22 07:00:13.000000Z]

  @rolling """
  #EXTM3U
  #EXT-X-VERSION:3
  #EXT-X-TARGETDURATION:10
  #EXT-X-MEDIA-SEQUENCE:6908
  #EXTINF:10.005311,
  live6908.ts
  #EXTINF:10.005311,
  live6909.ts
  """

  defp archive(file_names) do
    entries = Enum.map_join(file_names, "", &"#EXTINF:10.000000,\n#{&1}\n")

    "#EXTM3U\n#EXT-X-VERSION:3\n#EXT-X-TARGETDURATION:10\n#EXT-X-MEDIA-SEQUENCE:0\n" <> entries
  end

  defp create_stream!(feed, opts \\ []) do
    create_feed_stream!(
      [
        feed_id: feed.id,
        start_time: @start_time,
        bucket: feed.bucket,
        bucket_region: "us-west-2",
        playlist_timestamp: "1790060413",
        playlist_path: "/#{feed.node_name}/hls/1790060413/",
        playlist_m3u8_path: "/#{feed.node_name}/hls/1790060413/live.m3u8"
      ] ++ opts
    )
  end

  defp update_segments(feed_stream, archive_body) do
    get_body = fn %FeedStream{playlist_m3u8_path: path} ->
      if String.ends_with?(path, "/archive.m3u8"), do: {:ok, archive_body}, else: {:ok, @rolling}
    end

    feed_stream
    |> Ash.Changeset.for_update(:update_segments, %{}, context: %{get_manifest_body: get_body})
    |> Ash.update(authorize?: false)
  end

  defp file_names(feed_stream) do
    FeedSegment
    |> Ash.Query.filter(feed_stream_id == ^feed_stream.id)
    |> Ash.Query.sort(start_time: :asc)
    |> Ash.read!(authorize?: false)
    |> Enum.map(& &1.file_name)
  end

  setup do
    %{feed: create_feed!()}
  end

  test "records the archive's segments when live.m3u8 lists only recent ones", %{feed: feed} do
    feed_stream = create_stream!(feed)

    assert {:ok, _} =
             update_segments(feed_stream, archive(~w(live000.ts live001.ts live002.ts)))

    assert file_names(feed_stream) == ~w(live000.ts live001.ts live002.ts)

    # A later, longer archive adds only the new segments.
    assert {:ok, _} =
             update_segments(
               feed_stream,
               archive(~w(live000.ts live001.ts live002.ts live003.ts))
             )

    assert file_names(feed_stream) == ~w(live000.ts live001.ts live002.ts live003.ts)
  end

  test "leaves the duration of a stream that is still recording alone", %{feed: feed} do
    feed_stream = create_stream!(feed)

    assert {:ok, updated} = update_segments(feed_stream, archive(~w(live000.ts live001.ts)))
    assert updated.duration == nil
    assert updated.end_time == nil
  end

  test "lengthens the duration and end time of a stream that has ended", %{feed: feed} do
    next_stream = create_feed_stream!(feed_id: feed.id, start_time: ~U[2026-09-23 07:00:13Z])

    feed_stream =
      create_stream!(feed,
        next_feed_stream_id: next_stream.id,
        duration: Decimal.new("10.000000"),
        end_time: DateTime.add(@start_time, 10, :second)
      )

    assert {:ok, _} =
             update_segments(feed_stream, archive(~w(live000.ts live001.ts live002.ts)))

    reloaded = Ash.get!(FeedStream, feed_stream.id, authorize?: false)
    assert Decimal.equal?(reloaded.duration, Decimal.new(30))
    assert reloaded.end_time == DateTime.add(@start_time, 30, :second)
  end

  test "does not shorten the duration of a stream that has ended", %{feed: feed} do
    next_stream = create_feed_stream!(feed_id: feed.id, start_time: ~U[2026-09-23 07:00:13Z])

    feed_stream =
      create_stream!(feed,
        next_feed_stream_id: next_stream.id,
        duration: Decimal.new("60.000000"),
        end_time: DateTime.add(@start_time, 60, :second)
      )

    assert {:ok, _} = update_segments(feed_stream, archive(~w(live000.ts live001.ts)))

    reloaded = Ash.get!(FeedStream, feed_stream.id, authorize?: false)
    assert Decimal.equal?(reloaded.duration, Decimal.new(60))
  end

  test "records nothing from an archive that skips segments, and logs it once", %{feed: feed} do
    feed_stream = create_stream!(feed)
    skipping = archive(~w(live000.ts live001.ts live004.ts))

    log =
      capture_log(fn ->
        assert {:ok, _} = update_segments(feed_stream, skipping)
      end)

    assert log =~ "its playlist was refused"
    assert log =~ "skipped_segments"
    assert file_names(feed_stream) == []

    second_log =
      capture_log(fn ->
        assert {:ok, _} = update_segments(feed_stream, skipping)
      end)

    refute second_log =~ "its playlist was refused"
  end
end
