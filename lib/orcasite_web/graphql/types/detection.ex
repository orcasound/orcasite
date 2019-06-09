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

    field :timestamp, :datetime do
      resolve(fn detection, _, _ ->
        offset =
          detection.player_offset
          |> Decimal.to_float()
          |> Kernel.*(1000)
          |> round()

        time =
          detection.playlist_timestamp
          |> DateTime.from_unix!()
          |> DateTime.add(offset, :millisecond)

        {:ok, time}
      end)
    end
  end

  object :detection_with_lockout do
    field(:detection, :detection)
    field(:lockout_initial, :float)
    field(:lockout_remaining, :float)
  end
end
