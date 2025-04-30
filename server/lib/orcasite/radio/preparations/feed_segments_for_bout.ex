defmodule Orcasite.Radio.Preparations.FeedSegmentsForBout do
  use Ash.Resource.Preparation

  def prepare(query, _opts, _context) do
    bout_id =
      query
      |> Ash.Query.get_argument(:bout_id)

    bout = Orcasite.Radio.Bout |> Ash.get!(bout_id)

    query
    |> Ash.Query.filter(feed_id == ^bout.feed_id)
    |> Ash.Query.filter(
      expr(
        fragment(
          "(?) between (?) and (?)",
          start_time,
          ^bout.start_time,
          ^bout.end_time
        ) or
          fragment(
            "(?) between (?) and (?)",
            end_time,
            ^bout.start_time,
            ^bout.end_time
          )
      )
    )
  end
end
