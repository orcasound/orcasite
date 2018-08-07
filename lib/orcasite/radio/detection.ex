defmodule Orcasite.Radio.Detection do
  use Ecto.Schema
  import Ecto.Changeset


  schema "detections" do
    field :source_ip, :string
    field :playlist_timestamp, :integer
    field :player_offset, :decimal
    field :feed_id, :id

    timestamps()
  end

  @doc false
  def changeset(detection, attrs) do
    detection
    |> cast(attrs, [:feed_id, :playlist_timestamp, :player_offset, :source_ip])
    |> validate_required([:feed_id, :playlist_timestamp, :player_offset, :source_ip])
  end
end
