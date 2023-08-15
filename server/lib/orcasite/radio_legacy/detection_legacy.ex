defmodule Orcasite.RadioLegacy.Detection do
  use Ecto.Schema
  import Ecto.Changeset

  alias Orcasite.RadioLegacy.{Feed, Candidate}
  alias __MODULE__

  schema "detections_legacy" do
    field(:source_ip, :string)
    field(:playlist_timestamp, :integer)
    field(:player_offset, :decimal)
    field(:listener_count, :integer)
    field(:timestamp, :utc_datetime)
    field(:description, :string)

    belongs_to(:feed, Feed)
    belongs_to(:candidate, Candidate)

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
      :timestamp,
      :candidate_id,
      :description
    ])
    |> validate_required([:feed_id, :playlist_timestamp, :player_offset, :source_ip])
  end

  def from_json(attrs) do
    %Detection{}
    |> cast(attrs, Map.keys(Orcasite.Utils.atomize_keys(attrs)))
    |> apply_changes()
  end
end
