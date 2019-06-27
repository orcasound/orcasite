defmodule Orcasite.Radio.Candidate do
  use Ecto.Schema
  import Ecto.Changeset

  schema "candidates" do
    field(:detection_count, :integer)
    field(:min_time, :utc_datetime)
    field(:max_time, :utc_datetime)
    field(:feed_id, :id)

    timestamps()
  end

  @doc false
  def changeset(candidate, attrs) do
    candidate
    |> cast(attrs, [:detection_count, :min_time, :max_time, :feed_id])
    |> validate_required([:detection_count, :min_time, :max_time, :feed_id])
  end
end
