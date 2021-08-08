defmodule Orcasite.RadioTest do
  use Orcasite.DataCase

  alias Orcasite.Radio

  describe "feeds" do
    alias Orcasite.Radio.Feed

    @valid_attrs %{slug: "some slug", name: "some name", node_name: "some node_name"}
    @update_attrs %{slug: "some updated slug", name: "some updated name", node_name: "some updated node_name"}
    @invalid_attrs %{slug: nil, name: nil, node_name: nil}

    def feed_fixture(attrs \\ %{}) do
      {:ok, feed} =
        attrs
        |> Map.merge(@valid_attrs)
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
      assert feed.slug == "some slug"
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
      assert feed.slug == "some updated slug"
      assert feed.name == "some updated name"
      assert feed.node_name == "some updated node_name"
    end

    test "update_feed/2 with invalid data returns error changeset" do
      feed = feed_fixture()
      assert {:error, %Ecto.Changeset{}} = Radio.update_feed(feed, @invalid_attrs)
      assert feed == Radio.get_feed!(feed.id)
    end
  end

  describe "detections" do
    alias Orcasite.Radio.Detection

    @valid_attrs %{source_ip: "some source_ip", playlist_timestamp: 42, player_offset: Decimal.new("21421.678924"), feed_id: 1}
    @update_attrs %{source_ip: "some updated source_ip", playlist_timestamp: 43, player_offset: Decimal.new("21421.678924"), feed_id: 1}
    @invalid_attrs %{source_ip: nil, playlist_timestamp: nil, player_offset: nil, feed_id: nil}

    def detection_fixture(attrs \\ %{}) do
      {:ok, detection} =
        attrs
        |> Map.merge(@valid_attrs)
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
      assert detection.player_offset == Decimal.new("21421.678924")
      assert detection.feed_id == 1
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
      assert detection.player_offset == Decimal.new("21421.678924")
      assert detection.feed_id == 1
    end

    test "update_detection/2 with invalid data returns error changeset" do
      detection = detection_fixture()
      assert {:error, %Ecto.Changeset{}} = Radio.update_detection(detection, @invalid_attrs)
      assert detection == Radio.get_detection!(detection.id)
    end
  end

  describe "notification_events" do
    alias Orcasite.Notifications
    alias Orcasite.Notifications.NotificationEvents

    @valid_attrs %{candidate_id: 1, notified_by: "some email"}
    @update_attrs %{candidate_id: 1, notified_by: "some email"}
    @invalid_attrs %{candidate_id: nil, notified_by: nil}

    def notification_events_fixture(attrs \\ %{}) do
      {:ok, notification_events} =
        attrs
        |> Map.merge(@valid_attrs)
        |> Notifications.create_notification_event()

      notification_events
    end

    test "list_notification_event/0 returns all notification_events" do
      notification_event = notification_events_fixture()
      assert Notifications.list_notification_event() == [notification_event]
    end

    test "create_notification_event/1 with valid data creates a notification_event" do
      assert {:ok, %NotificationEvents{} = notification_event} = Notifications.create_notification_event(@valid_attrs)
      assert notification_event.candidate_id == 1
      assert notification_event.notified_by == "some email"
    end
  end
end
