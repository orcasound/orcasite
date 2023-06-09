defmodule Orcasite.Radio do
  @moduledoc """
  The Radio context.
  """

  import Ecto.Query, warn: false
  alias Orcasite.Repo
  alias Orcasite.Radio.{Feed, Detection, Candidate}

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

  def list_all_detections do
    Detection
    |> preload(:feed)
    |> Repo.all()
  end

  def list_detections(params \\ %{pagination: %{page: 1, page_size: 10}}) do
    Detection
    |> order_by(desc: :inserted_at)
    |> preload(:feed)
    |> Repo.paginate(page: params.pagination.page, page_size: params.pagination.page_size)
  end

  def get_detection!(id), do: Repo.get!(Detection, id)

  def create_detection(attrs \\ %{}) do
    %Detection{}
    |> Detection.changeset(merge_timestamp_attrs(attrs))
    |> Repo.insert()
  end

  def create_detection_with_candidate(%{feed_id: feed_id} = detection_attrs) do
    # Find or create candidate for detection, create detection
    # in ms
    within_ms = :timer.minutes(3)

    with %{timestamp: timestamp} when not is_nil(timestamp) <-
           merge_timestamp_attrs(detection_attrs),
         candidate <-
           find_or_create_relevant_candidate(
             %{feed_id: feed_id, timestamp: timestamp},
             within_ms
           ),
         {:ok, detection} <-
           create_detection(Map.merge(detection_attrs, %{candidate_id: candidate.id})),
         update_candidate(candidate, %{
           min_time: datetime_min(candidate.min_time, timestamp),
           max_time: datetime_max(candidate.max_time, timestamp),
           detection_count: candidate.detection_count + 1
         }) do
      {:ok, detection}
    end
  end

  def datetime_min(time_1, time_2) do
    case DateTime.compare(time_1, time_2) do
      :lt -> time_1
      _ -> time_2
    end
  end

  def datetime_max(time_1, time_2) do
    case DateTime.compare(time_1, time_2) do
      :gt -> time_1
      _ -> time_2
    end
  end

  def find_or_create_relevant_candidate(
        %{feed_id: feed_id, timestamp: timestamp},
        within_ms \\ 0
      ) do
    with min_time <- DateTime.add(timestamp, -within_ms, :millisecond),
         max_time <- DateTime.add(timestamp, within_ms, :millisecond),
         {:candidate, %Candidate{} = candidate} <-
           {:candidate,
            Repo.one(
              from(c in Candidate,
                where: c.feed_id == ^feed_id,
                where: ^max_time >= c.min_time and c.max_time >= ^min_time,
                limit: 1
              )
            )} do
      candidate
    else
      {:candidate, _} ->
        {:ok, candidate} =
          create_candidate(%{
            feed_id: feed_id,
            min_time: timestamp,
            max_time: timestamp,
            detection_count: 0
          })

        candidate
    end
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

  def list_candidates(params \\ %{pagination: %{page: 1, page_size: 10}}) do
    Candidate
    |> order_by(desc: :inserted_at)
    |> preload([:feed, :detections])
    |> Repo.paginate(page: params.pagination.page, page_size: params.pagination.page_size)
  end

  def update_detection_timestamp(detection) do
    attrs = Map.take(detection, [:playlist_timestamp, :player_offset])

    detection
    |> update_detection(merge_timestamp_attrs(attrs))
  end

  def create_candidate(attrs \\ %{}) do
    %Candidate{}
    |> Candidate.changeset(attrs)
    |> Repo.insert()
  end

  def update_candidate(candidate, attrs \\ %{}) do
    candidate
    |> Candidate.changeset(attrs)
    |> Repo.update()
  end
end
