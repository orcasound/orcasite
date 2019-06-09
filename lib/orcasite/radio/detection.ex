defmodule Orcasite.Radio.Detection do
  use Ecto.Schema
  import Ecto.Changeset

  alias Orcasite.Radio.Feed

  schema "detections" do
    field :source_ip, :string
    field :playlist_timestamp, :integer
    field :player_offset, :decimal
    field :listener_count, :integer

    belongs_to :feed, Feed

    timestamps()
  end

  @doc false
  def changeset(detection, attrs) do
    detection
    |> cast(attrs, [:feed_id, :playlist_timestamp, :player_offset, :source_ip, :listener_count])
    |> validate_required([:feed_id, :playlist_timestamp, :player_offset, :source_ip, :listener_count])
  end
end
