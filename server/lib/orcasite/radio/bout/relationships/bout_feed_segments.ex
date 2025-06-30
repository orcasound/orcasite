defmodule Orcasite.Radio.Bout.Relationships.BoutFeedSegments do
  use Ash.Resource.ManualRelationship
  require Ash.Query

  @impl Ash.Resource.ManualRelationship
  def load(records, _opts, context) do
    records = records |> Ash.load!([:start_time, :end_time])
    feed_ids = Enum.map(records, & &1.feed_id)

    min_time =
      records
      |> Enum.map(& &1.start_time)
      |> Enum.reject(&is_nil/1)
      |> Enum.min(DateTime, fn -> nil end)

    max_time =
      records
      |> Enum.map(& &1.end_time)
      |> Enum.reject(&is_nil/1)
      |> Enum.max(DateTime, fn -> nil end)

    if min_time && max_time do
      # Start with %{bout_1_id => [], bout_2_id => [], ...}
      start_acc = Enum.map(records, &{&1.id, []}) |> Enum.into(%{})

      # Get all feed segments relevant to the bouts
      Orcasite.Radio.FeedSegment
      |> Ash.Query.new()
      |> Ash.Query.filter(feed_id in ^feed_ids)
      |> Ash.Query.filter(
        fragment("(?) <= (?) AND (?) >= (?)", start_time, ^max_time, end_time, ^min_time)
      )
      |> Ash.stream!(Ash.Context.to_opts(context))
      # Pair up segments with any of the bouts
      |> Stream.transform(nil, fn segment, nil ->
        # Get bout ids for this segment
        bout_ids =
          records
          |> Enum.flat_map(fn bout ->
            if ranges_overlap?(bout, segment) do
              [bout.id]
            else
              []
            end
          end)

        {bout_ids |> Enum.map(&{&1, segment}), nil}
      end)
      # Create bout-grouped segments (i.e. %{bout_1_id => segments_1, bout_2_id => segments_2, ...})
      |> Enum.reduce(start_acc, fn {bout_id, segment}, acc ->
        acc
        |> update_in([bout_id], &[segment | &1])
      end)
      # Sort segments by time
      |> Enum.map(fn {bout_id, segments} -> {bout_id, Enum.sort_by(segments, & &1.start_time)} end)
      |> Enum.into(%{})
      |> then(&{:ok, &1})
    else
      {:ok, []}
    end
  end

  def ranges_overlap?(bout, segment) do
    if bout.start_time && bout.end_time && segment.start_time && segment.end_time do
      # startTime1 <= endTime2 && startTime2 <= endTime1
      DateTime.compare(bout.start_time, segment.end_time) != :gt &&
        DateTime.compare(segment.start_time, bout.end_time) != :gt
    end
  end
end
