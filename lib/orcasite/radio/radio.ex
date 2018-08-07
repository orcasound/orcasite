defmodule Orcasite.Radio do
  @moduledoc """
  The Radio context.
  """

  import Ecto.Query, warn: false
  alias Orcasite.Repo
  alias Orcasite.Radio.{Feed, Detection}

  def list_feeds do
    Repo.all(Feed)
  end

  def get_feed!(id), do: Repo.get!(Feed, id)

  def get_feed_by_slug(slug) do
    Feed
    |> where(slug: ^slug)
    |> limit(1)
    |> Repo.one()
  end

  def create_feed(attrs \\ %{}) do
    %Feed{}
    |> Feed.changeset(attrs)
    |> Repo.insert()
  end

  def update_feed(%Feed{} = feed, attrs) do
    feed
    |> Feed.changeset(attrs)
    |> Repo.update()
  end

  def delete_feed(%Feed{} = feed) do
    Repo.delete(feed)
  end

  def change_feed(%Feed{} = feed) do
    Feed.changeset(feed, %{})
  end

  def verify_can_submit_detection(
        feed_id,
        source_ip,
        timeout_seconds
      ) do
    Detection
    |> select(fragment("count(*) = 0"))
    |> where(source_ip: ^source_ip, feed_id: ^feed_id)
    |> where([d], d.inserted_at > ago(^timeout_seconds, "second"))
    |> Repo.one()
    |> case do
      true -> :ok
      false -> {:error, :recently_submitted}
    end
  end

  def list_detections do
    Repo.all(Detection)
  end

  def get_detection!(id), do: Repo.get!(Detection, id)

  def create_detection(attrs \\ %{}) do
    %Detection{}
    |> Detection.changeset(attrs)
    |> Repo.insert()
  end

  def update_detection(%Detection{} = detection, attrs) do
    detection
    |> Detection.changeset(attrs)
    |> Repo.update()
  end

  def delete_detection(%Detection{} = detection) do
    Repo.delete(detection)
  end

  def change_detection(%Detection{} = detection) do
    Detection.changeset(detection, %{})
  end
end
