defmodule Orcasite.Radio.Detection do
  use Ecto.Schema
  import Ecto.Changeset


  schema "detections" do
    field :source_ip, :string
    field :playlist_timestamp, :integer
    field :time, :utc_datetime
    field :feed_id, :id

    timestamps()
  end

  @doc false
  def changeset(detection, attrs) do
    detection
    |> cast(attrs, [:playlist_timestamp, :time, :source_ip])
    |> validate_required([:playlist_timestamp, :time, :source_ip])
  end
end
