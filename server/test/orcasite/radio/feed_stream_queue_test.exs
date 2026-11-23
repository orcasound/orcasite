defmodule Orcasite.Radio.FeedStreamQueueTest do
  use ExUnit.Case, async: true

  alias Orcasite.Radio.FeedStreamQueue

  @after_date ~U[2024-09-09 00:00:00Z]

  describe "one_per_folder/1" do
    test "keeps one path when live.m3u8 and archive.m3u8 arrive for the same folder" do
      paths = [
        %{m3u8_path: "rpi_orcasound_lab/hls/1794816013/live.m3u8", bucket: "audio-orcasound-net"},
        %{
          m3u8_path: "rpi_orcasound_lab/hls/1794816013/archive.m3u8",
          bucket: "audio-orcasound-net"
        },
        %{m3u8_path: "rpi_bush_point/hls/1794816018/live.m3u8", bucket: "audio-orcasound-net"}
      ]

      assert [
               %{m3u8_path: "rpi_orcasound_lab/hls/1794816013/live.m3u8"},
               %{m3u8_path: "rpi_bush_point/hls/1794816018/live.m3u8"}
             ] = FeedStreamQueue.one_per_folder(paths)
    end
  end

  describe "select_recent_timestamp/2" do
    test "accepts live.m3u8 and archive.m3u8 in a recent folder" do
      assert FeedStreamQueue.select_recent_timestamp(
               "rpi_orcasound_lab/hls/1794816013/live.m3u8",
               @after_date
             )

      assert FeedStreamQueue.select_recent_timestamp(
               "rpi_orcasound_lab/hls/1794816013/archive.m3u8",
               @after_date
             )
    end

    test "rejects other playlists and folders before the cutoff" do
      refute FeedStreamQueue.select_recent_timestamp(
               "rpi_orcasound_lab/hls/1794816013/other.m3u8",
               @after_date
             )

      refute FeedStreamQueue.select_recent_timestamp(
               "rpi_orcasound_lab/hls/1541027406/live.m3u8",
               @after_date
             )
    end
  end
end
