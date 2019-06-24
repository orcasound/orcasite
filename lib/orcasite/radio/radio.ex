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
        lockout_seconds
      ) do
    Detection
    |> where(source_ip: ^source_ip, feed_id: ^feed_id)
    |> where([d], d.inserted_at > ago(^lockout_seconds, "second"))
    |> order_by(desc: :inserted_at)
    |> limit(1)
    |> Repo.one()
    |> case do
      %Detection{inserted_at: inserted_at} ->
        diff = NaiveDateTime.diff(NaiveDateTime.utc_now(), inserted_at)
        remaining = lockout_seconds - diff
        {:error, %{lockout_remaining: remaining}}

      _ ->
        :ok
    end
  end

  @doc "Groups detections by timestamp - 3 minute buckets by default"
  def list_detection_groups(params \\ []) do
    # minutes
    interval = 3

    from(d in Detection,
      join: f in Feed,
      on: f.id == d.feed_id,
      select: %{
        feed: fragment("row_to_json(?)", f),
        detections: fragment("json_agg(?)", d),
        time_bucket:
          fragment(
            "(date_trunc('hour', timestamp) + (((date_part('minute', timestamp)::integer / ?::integer) * ?::integer) || 'minutes')::interval) at time zone 'UTC' as ts",
            ^interval,
            ^interval
          )
      },
      group_by: [fragment("ts"), f],
      order_by: fragment("ts desc")
    )
    |> Orcasite.Repo.all()

    # |> Orcasite.Utils.atomize_keys()
  end

  def list_detections do
    Detection
    |> preload(:feed)
    |> Repo.all()
  end

  def get_detection!(id), do: Repo.get!(Detection, id)

  def create_detection(attrs \\ %{}) do
    %Detection{}
    |> Detection.changeset(merge_timestamp_attrs(attrs))
    |> Repo.insert()
  end

  def update_detection(%Detection{} = detection, attrs) do
    detection
    |> Detection.changeset(merge_timestamp_attrs(attrs))
    |> Repo.update()
  end

  def merge_timestamp_attrs(%{playlist_timestamp: ts, player_offset: offset} = attrs)
      when is_nil(ts) or is_nil(offset),
      do: attrs

  def merge_timestamp_attrs(
        %{
          playlist_timestamp: playlist_timestamp,
          player_offset: player_offset
        } = attrs
      ) do
    epoch =
      playlist_timestamp
      |> case do
        ts when is_integer(ts) -> ts
        ts when is_binary(ts) -> String.to_integer(ts)
      end

    offset =
      player_offset
      |> case do
        %Decimal{} = offset -> round(Decimal.to_float(offset))
        offset when is_float(offset) -> round(offset)
      end

    timestamp =
      epoch
      |> DateTime.from_unix!()
      |> DateTime.add(offset)

    Map.merge(attrs, %{timestamp: timestamp})
  end

  def update_detection_timestamp(detection) do
    attrs = Map.take(detection, [:playlist_timestamp, :player_offset])

    detection
    |> update_detection(merge_timestamp_attrs(attrs))
  end

  def delete_detection(%Detection{} = detection) do
    Repo.delete(detection)
  end

  def change_detection(%Detection{} = detection) do
    Detection.changeset(detection, %{})
  end
end
