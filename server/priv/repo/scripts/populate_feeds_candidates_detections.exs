import Ecto.Query
require Ash.Query

alias Orcasite.Repo

feed_map =
  from(feed in Orcasite.RadioLegacy.Feed, order_by: [asc: feed.inserted_at])
  |> Repo.all()
  |> Enum.map(fn feed ->
    Orcasite.Radio.Feed
    |> Ash.Query.for_read(:read)
    |> Ash.Query.filter(slug == ^feed.slug)
    |> Orcasite.Radio.read()
    |> case do
      {:ok, [%Orcasite.Radio.Feed{} = new_feed | _]} ->
        {feed.id, new_feed.id}

      _ ->
        {:ok, new_feed, _} =
          Orcasite.Radio.Feed
          |> Ash.Changeset.for_create(:create, %{
            slug: feed.slug,
            name: feed.name,
            node_name: feed.node_name,
          })
          |> Ash.Changeset.force_change_attributes(%{
            inserted_at: feed.inserted_at,
            updated_at: feed.updated_at,
            location_point: feed.location_point
          })
          |> Orcasite.Radio.create(return_notifications?: true)

        {feed.id, new_feed.id}
    end
  end)
  |> Enum.to_list()
  |> Map.new()

# Iterate through candidates_legacy, create a new candidate
from(cand in Orcasite.RadioLegacy.Candidate,
  order_by: [asc: cand.inserted_at],
  where: not is_nil(cand.min_time) and not is_nil(cand.max_time)
)
|> Repo.all()
|> Enum.map(fn candidate ->
  feed_id = Map.get(feed_map, candidate.feed_id)
  Orcasite.Radio.Candidate
  |> Ash.Query.for_read(:read)
  |> Ash.Query.filter(
    min_time == ^candidate.min_time and max_time == ^candidate.max_time and
      feed_id == ^feed_id
  )
  |> Orcasite.Radio.read()
  |> case do
    {:ok, [%Orcasite.Radio.Candidate{} | _]} ->
      nil

    _ ->
      Orcasite.Radio.Candidate
      |> Ash.Changeset.for_create(:create, %{
        detection_count: candidate.detection_count,
        min_time: candidate.min_time,
        max_time: candidate.max_time,
        feed: %{id: feed_id}
      })
      |> Ash.Changeset.force_change_attributes(%{
        inserted_at: candidate.inserted_at,
        updated_at: candidate.updated_at
      })
      |> Orcasite.Radio.create(return_notifications?: true)
  end
end)

# Iterate through each detection_legacy, associate with candidate that matches
# the timestamp and the feed_id
from(det in Orcasite.RadioLegacy.Detection, order_by: [asc: det.inserted_at])
|> Repo.all()
|> Enum.map(fn detection ->
  feed_id = Map.get(feed_map, detection.feed_id)
  Orcasite.Radio.Detection
  |> Ash.Query.for_read(:read)
  |> Ash.Query.filter(
    feed_id == ^feed_id and timestamp == ^detection.timestamp and
      playlist_timestamp == ^detection.playlist_timestamp and
      player_offset == ^detection.player_offset
  )
  |> Orcasite.Radio.read()
  |> case do
    {:ok, [%Orcasite.Radio.Detection{} | _]} ->
      nil

    _ ->
      [candidate] =
        Orcasite.Radio.Candidate
        |> Ash.Query.for_read(:read)
        |> Ash.Query.filter(
          feed_id == ^feed_id and min_time <= ^detection.timestamp and
            max_time >= ^detection.timestamp
        )
        |> Orcasite.Radio.read!()

      Orcasite.Radio.Detection
      |> Ash.Changeset.for_create(
        :create,
        Map.take(detection, [
          :playlist_timestamp,
          :player_offset,
          :source_ip,
          :listener_count,
          :timestamp,
          :description
        ])
        |> Map.merge(%{
          candidate: %{id: candidate.id},
          feed: %{id: feed_id}
        })
      )
      |> Ash.Changeset.force_change_attributes(%{
        inserted_at: detection.inserted_at,
        updated_at: detection.updated_at
      })
      |> Orcasite.Radio.create(return_notifications?: true)
    end
end)
