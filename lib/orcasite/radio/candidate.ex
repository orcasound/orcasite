defmodule Orcasite.Radio.Candidate do
  use Ecto.Schema
  import Ecto.Changeset


  schema "candidates" do
    field :detection_count, :integer
    field :timestamp, :utc_datetime
    field :feed_id, :id

    timestamps()
  end

  @doc false
  def changeset(candidate, attrs) do
    candidate
    |> cast(attrs, [:detection_count, :timestamp])
    |> validate_required([:detection_count, :timestamp])
  end
end
