defmodule Orcasite.Radio.FeedStreamTest do
  use ExUnit.Case, async: true

  alias Orcasite.Radio.FeedStream

  @start_time ~U[2026-09-22 07:00:13.000000Z]

  @feed_stream %FeedStream{
    start_time: @start_time,
    bucket: "audio-orcasound-net",
    bucket_region: "us-west-2",
    cloudfront_url: "https://audio-orcasound-net.s3.amazonaws.com",
    playlist_timestamp: "1790060413",
    playlist_path: "/rpi_orcasound_lab/hls/1790060413/",
    playlist_m3u8_path: "/rpi_orcasound_lab/hls/1790060413/live.m3u8"
  }

  # ffmpeg `-f segment` muxer, as written by nodes before node_2026
  @legacy_manifest """
  #EXTM3U
  #EXT-X-VERSION:3
  #EXT-X-MEDIA-SEQUENCE:0
  #EXT-X-ALLOW-CACHE:YES
  #EXT-X-TARGETDURATION:11
  #EXTINF:10.005422,
  live000.ts
  #EXTINF:10.005411,
  live001.ts
  #EXTINF:9.984056,
  live002.ts
  """

  # ffmpeg `-f hls` muxer with `-hls_flags program_date_time`, as written by node_2026
  @program_date_time_manifest """
  #EXTM3U
  #EXT-X-VERSION:3
  #EXT-X-TARGETDURATION:10
  #EXT-X-MEDIA-SEQUENCE:0
  #EXTINF:10.005333,
  #EXT-X-PROGRAM-DATE-TIME:2026-09-22T00:00:14.675-0700
  live000.ts
  #EXTINF:10.005333,
  #EXT-X-PROGRAM-DATE-TIME:2026-09-22T00:00:24.680-0700
  live001.ts
  #EXTINF:10.005333,
  #EXT-X-PROGRAM-DATE-TIME:2026-09-22T00:00:34.685-0700
  live002.ts
  """

  # ffmpeg `-f hls` muxer with `-hls_list_size 10`, as written by node_2026 since
  # October 2026: live.m3u8 keeps only the most recent segments (#1092)
  @rolling_manifest """
  #EXTM3U
  #EXT-X-VERSION:3
  #EXT-X-TARGETDURATION:10
  #EXT-X-MEDIA-SEQUENCE:6908
  #EXTINF:10.005311,
  #EXT-X-PROGRAM-DATE-TIME:2026-09-22T19:11:35.163-0700
  live6908.ts
  #EXTINF:10.005311,
  #EXT-X-PROGRAM-DATE-TIME:2026-09-22T19:11:45.168-0700
  live6909.ts
  """

  # archive.m3u8, written beside the rolling live.m3u8: every segment since the folder started
  @archive_manifest """
  #EXTM3U
  #EXT-X-VERSION:3
  #EXT-X-TARGETDURATION:10
  #EXT-X-MEDIA-SEQUENCE:0
  #EXTINF:10.005333,
  #EXT-X-PROGRAM-DATE-TIME:2026-09-22T00:00:15.162-0700
  live000.ts
  #EXTINF:10.005289,
  #EXT-X-PROGRAM-DATE-TIME:2026-09-22T00:00:25.167-0700
  live001.ts
  """

  describe "parse_manifest/3" do
    test "parses the legacy segment-muxer format" do
      assert {:ok, segments} = FeedStream.parse_manifest(@legacy_manifest, @feed_stream)

      assert Enum.map(segments, & &1.file_name) == ["live000.ts", "live001.ts", "live002.ts"]

      assert Enum.map(segments, & &1.duration) == [
               Decimal.new("10.005422"),
               Decimal.new("10.005411"),
               Decimal.new("9.984056")
             ]
    end

    test "parses the program-date-time format" do
      assert {:ok, segments} =
               FeedStream.parse_manifest(@program_date_time_manifest, @feed_stream)

      assert Enum.map(segments, & &1.file_name) == ["live000.ts", "live001.ts", "live002.ts"]
    end

    test "records each segment's program date time in UTC without shifting its computed times" do
      assert {:ok, [first, second | _]} =
               FeedStream.parse_manifest(@program_date_time_manifest, @feed_stream)

      assert first.program_date_time == ~U[2026-09-22 07:00:14.675Z]
      assert second.program_date_time == ~U[2026-09-22 07:00:24.680Z]
      assert first.start_time == @start_time
    end

    test "leaves program date time nil for the legacy format" do
      assert {:ok, segments} = FeedStream.parse_manifest(@legacy_manifest, @feed_stream)
      assert Enum.all?(segments, &is_nil(&1.program_date_time))
    end

    test "derives segment times by accumulating durations from the stream start" do
      assert {:ok, [first, second, third]} =
               FeedStream.parse_manifest(@legacy_manifest, @feed_stream)

      assert first.start_time == @start_time
      assert first.end_time == DateTime.add(@start_time, 10_005, :millisecond)
      assert second.start_time == first.end_time
      assert second.end_time == DateTime.add(@start_time, 20_011, :millisecond)
      assert third.start_time == second.end_time
      # 10.005422 + 10.005411 + 9.984056 = 29.994889 s
      assert third.end_time == DateTime.add(@start_time, 29_995, :millisecond)
    end

    test "fills in stream-derived attributes" do
      assert {:ok, [segment | _]} = FeedStream.parse_manifest(@legacy_manifest, @feed_stream)

      assert segment.segment_path == "/rpi_orcasound_lab/hls/1790060413/live000.ts"
      assert segment.playlist_path == @feed_stream.playlist_path
      assert segment.playlist_m3u8_path == @feed_stream.playlist_m3u8_path
      assert segment.playlist_timestamp == @feed_stream.playlist_timestamp
      assert segment.bucket == @feed_stream.bucket
      assert segment.bucket_region == @feed_stream.bucket_region
      assert segment.cloudfront_url == @feed_stream.cloudfront_url
      assert segment.feed_stream == @feed_stream
      assert segment.feed == nil
    end

    test "returns an error for a body that is not a media playlist" do
      assert {:error, _} = FeedStream.parse_manifest("", @feed_stream)
      assert {:error, _} = FeedStream.parse_manifest("<html>Access Denied</html>", @feed_stream)
    end

    test "refuses a playlist that does not start with the folder's first segment" do
      assert {:error, {:rolling_playlist, 6908}} =
               FeedStream.parse_manifest(@rolling_manifest, @feed_stream)
    end
  end

  describe "request_and_parse_manifest/3" do
    @live_path "/rpi_orcasound_lab/hls/1790060413/live.m3u8"
    @archive_path "/rpi_orcasound_lab/hls/1790060413/archive.m3u8"

    defp fake_s3(bodies) do
      test_pid = self()

      fn %FeedStream{playlist_m3u8_path: path} ->
        send(test_pid, {:fetched, path})
        Map.get(bodies, path, {:error, %{status_code: 404}})
      end
    end

    test "parses live.m3u8 when it lists every segment, without fetching the archive" do
      get_body = fake_s3(%{@live_path => {:ok, @legacy_manifest}})

      assert {:ok, [_, _, _]} = FeedStream.request_and_parse_manifest(@feed_stream, nil, get_body)
      assert_received {:fetched, @live_path}
      refute_received {:fetched, @archive_path}
    end

    test "reads archive.m3u8 when live.m3u8 lists only recent segments" do
      get_body =
        fake_s3(%{
          @live_path => {:ok, @rolling_manifest},
          @archive_path => {:ok, @archive_manifest}
        })

      assert {:ok, [first, second]} =
               FeedStream.request_and_parse_manifest(@feed_stream, nil, get_body)

      assert first.file_name == "live000.ts"
      assert first.start_time == @start_time
      assert second.file_name == "live001.ts"
      assert second.start_time == DateTime.add(@start_time, 10_005, :millisecond)
    end

    test "returns the error when live.m3u8 lists only recent segments and there is no archive" do
      get_body = fake_s3(%{@live_path => {:ok, @rolling_manifest}})

      assert {:error, %{status_code: 404}} =
               FeedStream.request_and_parse_manifest(@feed_stream, nil, get_body)
    end

    test "refuses an archive that also lists only recent segments" do
      get_body =
        fake_s3(%{
          @live_path => {:ok, @rolling_manifest},
          @archive_path => {:ok, @rolling_manifest}
        })

      assert {:error, {:rolling_playlist, 6908}} =
               FeedStream.request_and_parse_manifest(@feed_stream, nil, get_body)
    end
  end
end
