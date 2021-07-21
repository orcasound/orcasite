defmodule Orcasite.Radio.Candidate do
  use Ecto.Schema
  import Ecto.Changeset

  alias Orcasite.Radio.{Detection, Feed}
  alias Orcasite.Notifications.NotificationEvents

  schema "candidates" do
    field(:detection_count, :integer)
    field(:min_time, :utc_datetime)
    field(:max_time, :utc_datetime)

    has_many(:detections, Detection)
    belongs_to(:feed, Feed)
    has_one(:notification_events, NotificationEvents, references: :id, foreign_key: :id)

    timestamps()
  end

  @doc false
  def changeset(candidate, attrs) do
    candidate
    |> cast(attrs, [:detection_count, :min_time, :max_time, :feed_id])
    |> validate_required([:detection_count, :min_time, :max_time, :feed_id])
  end
end
