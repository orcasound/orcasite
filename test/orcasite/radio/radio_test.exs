defmodule Orcasite.RadioTest do
  use Orcasite.DataCase

  alias Orcasite.Radio

  describe "feeds" do
    alias Orcasite.Radio.Feed

    @valid_attrs %{location_name: "some location_name", name: "some name", node_name: "some node_name"}
    @update_attrs %{location_name: "some updated location_name", name: "some updated name", node_name: "some updated node_name"}
    @invalid_attrs %{location_name: nil, name: nil, node_name: nil}

    def feed_fixture(attrs \\ %{}) do
      {:ok, feed} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Radio.create_feed()

      feed
    end

    test "list_feeds/0 returns all feeds" do
      feed = feed_fixture()
      assert Radio.list_feeds() == [feed]
    end

    test "get_feed!/1 returns the feed with given id" do
      feed = feed_fixture()
      assert Radio.get_feed!(feed.id) == feed
    end

    test "create_feed/1 with valid data creates a feed" do
      assert {:ok, %Feed{} = feed} = Radio.create_feed(@valid_attrs)
      assert feed.location_name == "some location_name"
      assert feed.name == "some name"
      assert feed.node_name == "some node_name"
    end

    test "create_feed/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Radio.create_feed(@invalid_attrs)
    end

    test "update_feed/2 with valid data updates the feed" do
      feed = feed_fixture()
      assert {:ok, feed} = Radio.update_feed(feed, @update_attrs)
      assert %Feed{} = feed
      assert feed.location_name == "some updated location_name"
      assert feed.name == "some updated name"
      assert feed.node_name == "some updated node_name"
    end

    test "update_feed/2 with invalid data returns error changeset" do
      feed = feed_fixture()
      assert {:error, %Ecto.Changeset{}} = Radio.update_feed(feed, @invalid_attrs)
      assert feed == Radio.get_feed!(feed.id)
    end

    test "delete_feed/1 deletes the feed" do
      feed = feed_fixture()
      assert {:ok, %Feed{}} = Radio.delete_feed(feed)
      assert_raise Ecto.NoResultsError, fn -> Radio.get_feed!(feed.id) end
    end

    test "change_feed/1 returns a feed changeset" do
      feed = feed_fixture()
      assert %Ecto.Changeset{} = Radio.change_feed(feed)
    end
  end

  describe "detections" do
    alias Orcasite.Radio.Detection

    @valid_attrs %{source_ip: "some source_ip", playlist_timestamp: 42, time: "2010-04-17 14:00:00.000000Z"}
    @update_attrs %{source_ip: "some updated source_ip", playlist_timestamp: 43, time: "2011-05-18 15:01:01.000000Z"}
    @invalid_attrs %{source_ip: nil, playlist_timestamp: nil, time: nil}

    def detection_fixture(attrs \\ %{}) do
      {:ok, detection} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Radio.create_detection()

      detection
    end

    test "list_detections/0 returns all detections" do
      detection = detection_fixture()
      assert Radio.list_detections() == [detection]
    end

    test "get_detection!/1 returns the detection with given id" do
      detection = detection_fixture()
      assert Radio.get_detection!(detection.id) == detection
    end

    test "create_detection/1 with valid data creates a detection" do
      assert {:ok, %Detection{} = detection} = Radio.create_detection(@valid_attrs)
      assert detection.source_ip == "some source_ip"
      assert detection.playlist_timestamp == 42
      assert detection.time == DateTime.from_naive!(~N[2010-04-17 14:00:00.000000Z], "Etc/UTC")
    end

    test "create_detection/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Radio.create_detection(@invalid_attrs)
    end

    test "update_detection/2 with valid data updates the detection" do
      detection = detection_fixture()
      assert {:ok, detection} = Radio.update_detection(detection, @update_attrs)
      assert %Detection{} = detection
      assert detection.source_ip == "some updated source_ip"
      assert detection.playlist_timestamp == 43
      assert detection.time == DateTime.from_naive!(~N[2011-05-18 15:01:01.000000Z], "Etc/UTC")
    end

    test "update_detection/2 with invalid data returns error changeset" do
      detection = detection_fixture()
      assert {:error, %Ecto.Changeset{}} = Radio.update_detection(detection, @invalid_attrs)
      assert detection == Radio.get_detection!(detection.id)
    end

    test "delete_detection/1 deletes the detection" do
      detection = detection_fixture()
      assert {:ok, %Detection{}} = Radio.delete_detection(detection)
      assert_raise Ecto.NoResultsError, fn -> Radio.get_detection!(detection.id) end
    end

    test "change_detection/1 returns a detection changeset" do
      detection = detection_fixture()
      assert %Ecto.Changeset{} = Radio.change_detection(detection)
    end
  end
end
