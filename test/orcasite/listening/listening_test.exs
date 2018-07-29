defmodule Orcasite.ListeningTest do
  use Orcasite.DataCase

  alias Orcasite.Listening

  describe "feeds" do
    alias Orcasite.Listening.Feed

    @valid_attrs %{location_name: "some location_name", name: "some name", node_name: "some node_name"}
    @update_attrs %{location_name: "some updated location_name", name: "some updated name", node_name: "some updated node_name"}
    @invalid_attrs %{location_name: nil, name: nil, node_name: nil}

    def feed_fixture(attrs \\ %{}) do
      {:ok, feed} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Listening.create_feed()

      feed
    end

    test "list_feeds/0 returns all feeds" do
      feed = feed_fixture()
      assert Listening.list_feeds() == [feed]
    end

    test "get_feed!/1 returns the feed with given id" do
      feed = feed_fixture()
      assert Listening.get_feed!(feed.id) == feed
    end

    test "create_feed/1 with valid data creates a feed" do
      assert {:ok, %Feed{} = feed} = Listening.create_feed(@valid_attrs)
      assert feed.location_name == "some location_name"
      assert feed.name == "some name"
      assert feed.node_name == "some node_name"
    end

    test "create_feed/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Listening.create_feed(@invalid_attrs)
    end

    test "update_feed/2 with valid data updates the feed" do
      feed = feed_fixture()
      assert {:ok, feed} = Listening.update_feed(feed, @update_attrs)
      assert %Feed{} = feed
      assert feed.location_name == "some updated location_name"
      assert feed.name == "some updated name"
      assert feed.node_name == "some updated node_name"
    end

    test "update_feed/2 with invalid data returns error changeset" do
      feed = feed_fixture()
      assert {:error, %Ecto.Changeset{}} = Listening.update_feed(feed, @invalid_attrs)
      assert feed == Listening.get_feed!(feed.id)
    end

    test "delete_feed/1 deletes the feed" do
      feed = feed_fixture()
      assert {:ok, %Feed{}} = Listening.delete_feed(feed)
      assert_raise Ecto.NoResultsError, fn -> Listening.get_feed!(feed.id) end
    end

    test "change_feed/1 returns a feed changeset" do
      feed = feed_fixture()
      assert %Ecto.Changeset{} = Listening.change_feed(feed)
    end
  end
end
