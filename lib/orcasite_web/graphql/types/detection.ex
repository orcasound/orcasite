defmodule OrcasiteWeb.Types.Detection do
  @moduledoc """
  GraphQL types for feeds
  """
  use Absinthe.Schema.Notation

  object :detection do
    field(:id, :id)
    field(:feed, :feed)
    field(:playlist_timestamp, :integer)
    field(:source_ip, :string)
    field(:player_offset, :decimal)
    field(:listener_count, :integer)
    field(:timestamp, :datetime)
  end

  object :detection_with_lockout do
    field(:detection, :detection)
    field(:lockout_initial, :float)
    field(:lockout_remaining, :float)
  end

  object :candidate do
    field(:id, :id)
    field(:min_time, :datetime)
    field(:max_time, :datetime)
    field(:detection_count, :integer)
    field(:detections, list_of(:detection))
    field(:feed, :feed)
  end

  @desc "Pagination version of candidates"
  object :candidates do
    field(:meta, :pagination_meta)
    field(:entries, list_of(:candidate))
  end
end
