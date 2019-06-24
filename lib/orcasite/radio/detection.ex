defmodule Orcasite.Radio.Detection do
  use Ecto.Schema
  import Ecto.Changeset

  alias Orcasite.Radio.Feed

  schema "detections" do
    field(:source_ip, :string)
    field(:playlist_timestamp, :integer)
    field(:player_offset, :decimal)
    field(:listener_count, :integer)
    field(:timestamp, :utc_datetime)

    belongs_to(:feed, Feed)

    timestamps()
  end

  @doc false
  def changeset(detection, attrs) do
    detection
    |> cast(attrs, [
      :feed_id,
      :playlist_timestamp,
      :player_offset,
      :source_ip,
      :listener_count,
      :timestamp
    ])
    |> validate_required([:feed_id, :playlist_timestamp, :player_offset, :source_ip])
  end
end
