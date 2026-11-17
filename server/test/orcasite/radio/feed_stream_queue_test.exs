defmodule Orcasite.Radio.FeedStreamQueueTest do
  use ExUnit.Case, async: true

  alias Orcasite.Radio.FeedStreamQueue

  @after_date ~U[2024-09-09 00:00:00Z]

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
