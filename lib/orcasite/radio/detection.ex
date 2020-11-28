defmodule Orcasite.Radio.Detection do
  use Ecto.Schema
  import Ecto.Changeset

  alias Orcasite.Radio.{Feed, Candidate}
  alias __MODULE__

  schema "detections" do
    field(:source_ip, :string)
    field(:playlist_timestamp, :integer)
    field(:player_offset, :decimal)
    field(:listener_count, :integer)
    field(:timestamp, :utc_datetime)
    field(:description, :string)
    field(:type, :string)

    belongs_to(:feed, Feed)
    belongs_to(:candidate, Candidate)

    timestamps()
  end

  @spec changeset(
          {map, map} | %{:__struct__ => atom | %{__changeset__: map}, optional(atom) => any},
          :invalid | %{optional(:__struct__) => none, optional(atom | binary) => any}
        ) :: Ecto.Changeset.t()
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
