defmodule Orcasite.Radio.Candidate do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshUUID],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Radio.{Detection, Feed}

  postgres do
    table "candidates"
    repo Orcasite.Repo

    custom_indexes do
      index [:min_time]
      index [:max_time]
    end
  end

  attributes do
    uuid_attribute :id, prefix: "cand"

    attribute :detection_count, :integer
    attribute :min_time, :utc_datetime_usec
    attribute :max_time, :utc_datetime_usec

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  actions do
    defaults [:update, :destroy]

    read :read do
      primary? true
      prepare build(load: [:uuid])
    end

    create :create do
      primary? true

      argument :detections, {:array, :map}
      argument :feed, :map

      change manage_relationship(:feed, type: :append)
      change manage_relationship(:detections, type: :append)
    end
  end

  calculations do
    calculate :uuid, :string, {Orcasite.Radio.Calculations.DecodeUUID, []}
  end

  relationships do
    has_many :detections, Detection

    belongs_to :feed, Feed do
      attribute_type :integer
    end
  end

  admin do
    table_columns [:id, :detection_count, :feed, :min_time, :max_time, :inserted_at]
  end
end
