defmodule Orcasite.Radio.Calculations.BoutExportJson do
  use Ash.Resource.Calculation

  def load(_, _, _), do: [:feed_segments, feed: [:slug]]

  def calculate(records, _opts, _context) do
    records
    |> Enum.map(fn bout ->
      segments = bout.feed_segments

      bout
      |> Map.take([:id, :name, :start_time, :end_time, :duration, :category])
      |> Map.put(
        :feed_segments,
        segments
        |> Enum.map(fn seg ->
          seg
          |> Ash.load!(:s3_url)
          |> Map.take([:id, :feed_id, :start_time, :end_time, :duration, :file_name, :s3_url])
          |> Map.put(:feed_slug, bout.feed.slug)
        end)
      )
      |> Jason.encode!()
    end)
  end
end
