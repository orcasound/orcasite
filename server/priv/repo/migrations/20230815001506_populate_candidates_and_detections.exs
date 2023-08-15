defmodule Orcasite.Repo.Migrations.PopulateCandidatesAndDetections do
  use Ecto.Migration
  import Ecto.Query
  require Ash.Query

  alias Orcasite.Repo

  def up do
    # Iterate through candidates_legacy, create a new candidate
    Repo.transaction(
      fn ->
        from(cand in Orcasite.RadioLegacy.Candidate,
          order_by: [asc: cand.inserted_at],
          where: not is_nil(cand.min_time) and not is_nil(cand.max_time)
        )
        |> Repo.stream()
        |> Stream.map(fn candidate ->
          Orcasite.Radio.Candidate
          |> Ash.Query.for_read(:read)
          |> Ash.Query.filter(
            min_time == ^candidate.min_time and max_time == ^candidate.max_time and
              feed_id == ^candidate.feed_id
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
                feed: %{id: candidate.feed_id}
              })
              |> Ash.Changeset.force_change_attributes(%{
                inserted_at: candidate.inserted_at,
                updated_at: candidate.updated_at
              })
              |> Orcasite.Radio.create(return_notifications?: true)
          end
        end)
        |> Stream.run()
      end,
      timeout: :infinity
    )

    # Iterate through each detection_legacy, associate with candidate that matches
    # the timestamp and the feed_id
    Repo.transaction(
      fn ->
        from(det in Orcasite.RadioLegacy.Detection, order_by: [asc: det.inserted_at])
        |> Repo.stream()
        |> Stream.map(fn detection ->
          Orcasite.Radio.Detection
          |> Ash.Query.for_read(:read)
          |> Ash.Query.filter(
            feed_id == ^detection.feed_id and timestamp == ^detection.timestamp and
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
                  feed_id == ^detection.feed_id and min_time <= ^detection.timestamp and
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
                  feed: %{id: detection.feed_id}
                })
              )
              |> Ash.Changeset.force_change_attributes(%{
                inserted_at: detection.inserted_at,
                updated_at: detection.updated_at
              })
              |> Orcasite.Radio.create(return_notifications?: true)
          end
        end)
        |> Stream.run()
      end,
      timeout: :infinity
    )
  end

  def down do
  end
end
