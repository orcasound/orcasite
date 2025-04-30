defmodule Orcasite.Radio.Bout.Relationships.BoutFeedSegments do
  use Ash.Resource.ManualRelationship
  require Ash.Query

  @impl true
  def load(records, _opts, context) do
    feed_ids = Enum.map(records, & &1.feed_id)
    min_time = Enum.map(records, & &1.start_time) |> Enum.min(Datetime, fn -> nil end)
    max_time = Enum.map(records, & &1.end_time) |> Enum.max(Datetime, fn -> nil end)

    # Start with %{bout_1_id => [], bout_2_id => [], ...}
    start_acc = Enum.map(records, &{&1.id, []}) |> Enum.into(%{})

    # Get all feed segments relevant to the bouts
    Orcasite.Radio.FeedSegment
    |> Ash.Query.new()
    |> Ash.Query.filter(feed_id in ^feed_ids)
    |> Ash.Query.filter(
      fragment("(?) between (?) and (?)", start_time, ^min_time, ^max_time) or
        fragment("(?) between (?) and (?)", end_time, ^min_time, ^max_time)
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
  end

  def ranges_overlap?(bout, segment) do
    if bout.start_time && bout.end_time && segment.start_time && segment.end_time do
      # startTime1 <= endTime2 && startTime2 <= endTime1
      DateTime.compare(bout.start_time, segment.end_time) != :gt &&
        DateTime.compare(segment.start_time, bout.end_time) != :gt
    end
  end
end
