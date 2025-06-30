defmodule Orcasite.Radio.Detection.Changes.UpdateCandidate do
  use Ash.Resource.Change

  alias Orcasite.Radio.Candidate

  @impl Ash.Resource.Change
  def change(changeset, _opts, _context) do
    changeset
    |> Ash.Changeset.after_action(fn _change, detection ->
      # Find or create candidate, update detection with candidate
      candidate =
        Candidate
        |> Ash.Query.for_read(:find_nearby_candidate, %{
          timestamp: detection.timestamp,
          feed_id: detection.feed_id,
          category: detection.category
        })
        |> Ash.read!()
        |> case do
          [] ->
            Candidate
            |> Ash.Changeset.for_create(:create, %{
              min_time: detection.timestamp,
              max_time: detection.timestamp,
              detection_count: 1,
              feed: %{id: detection.feed_id},
              category: detection.category
            })
            |> Ash.create!()

          [candidate] ->
            candidate
            |> Ash.Changeset.for_update(:update, %{
              detection_count: candidate.detection_count + 1,
              min_time: datetime_min(candidate.min_time, detection.timestamp),
              max_time: datetime_max(candidate.max_time, detection.timestamp)
            })
            |> Ash.update!(authorize?: false)
        end

      detection
      |> Ash.Changeset.for_update(:update_candidate, %{candidate: candidate})
      |> Ash.update(authorize?: false)
    end)
  end

  defp datetime_min(time_1, time_2) do
    case DateTime.compare(time_1, time_2) do
      :lt -> time_1
      _ -> time_2
    end
  end

  defp datetime_max(time_1, time_2) do
    case DateTime.compare(time_1, time_2) do
      :gt -> time_1
      _ -> time_2
    end
  end
end
