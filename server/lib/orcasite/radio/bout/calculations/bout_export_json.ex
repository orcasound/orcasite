defmodule Orcasite.Radio.Bout.Calculations.BoutExportJson do
  use Ash.Resource.Calculation

  @impl Ash.Resource.Calculation
  def load(_, _, _),
    do: [
      :id,
      :name,
      :start_time,
      :end_time,
      :duration,
      :category,
      feed_segments: [:id, :feed_id, :start_time, :end_time, :duration, :file_name, :s3_url],
      feed: [:slug]
    ]

  @impl Ash.Resource.Calculation
  def calculate(records, _opts, _context) do
    records
    |> Enum.map(fn bout ->
      segments = bout.feed_segments

      bout
      |> Map.take([:id, :name, :start_time, :end_time, :duration, :category])
      |> Map.put(:feed_segments_count, Enum.count(segments))
      |> Map.put(
        :feed_segments,
        segments
        |> Enum.map(fn seg ->
          seg
          |> Map.take([:id, :feed_id, :start_time, :end_time, :duration, :file_name, :s3_url])
          |> Map.put(:feed_slug, bout.feed.slug)
        end)
      )
      |> Jason.encode!()
    end)
    |> then(&{:ok, &1})
  end
end
