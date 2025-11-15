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
        |> Ash.read!(load: :detections, authorize?: false)
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

          candidates ->
            # Choose candidate that's closest to detection's timestamps
            candidate =
              Enum.sort_by(
                candidates,
                fn candidate ->
                  [
                    {candidate.min_time, detection.start_time},
                    {candidate.min_time, detection.end_time},
                    {candidate.max_time, detection.start_time},
                    {candidate.min_time, detection.end_time}
                  ]
                  |> Enum.map(fn {t1, t2} ->
                    Kernel.abs(DateTime.diff(t1, t2, :millisecond))
                  end)
                  |> Enum.min()
                end,
                :asc
              )
              |> Enum.at(0)

            detections =
              candidate.detections
              |> Kernel.++([detection])
              |> Enum.uniq_by(& &1.id)

            detection_count = Enum.count(detections)

            min_time =
              tl(detections)
              |> Enum.reduce(hd(detections).timestamp, &datetime_min(&1.timestamp, &2))

            max_time =
              tl(detections)
              |> Enum.reduce(hd(detections).timestamp, &datetime_max(&1.timestamp, &2))

            candidate
            |> Ash.Changeset.for_update(:update, %{
              detection_count: detection_count,
              min_time: min_time,
              max_time: max_time
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
