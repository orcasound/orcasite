defmodule Orcasite.Radio.Detection do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Radio.{Feed, Candidate}

  postgres do
    table "detections"
    repo Orcasite.Repo
    custom_indexes do
      index [:playlist_timestamp]
      index [:player_offset]
      index [:timestamp]
      index [:description]
    end
  end

  attributes do
    uuid_attribute :id, prefix: "det"

    attribute :source_ip, :string
    attribute :playlist_timestamp, :integer
    attribute :player_offset, :decimal
    attribute :listener_count, :integer
    attribute :timestamp, :utc_datetime_usec
    attribute :description, :string

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  actions do
    defaults [:update, :destroy]

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end
      prepare build(load: [:uuid])
    end

    read :read do
      primary? true
      prepare build(load: [:uuid])
    end

    create :create do
      primary? true
      argument :candidate, :map
      argument :feed, :map

      change manage_relationship(:candidate, type: :append)
      change manage_relationship(:feed, type: :append)
    end

    create :submit_detection do
      accept [:playlist_timestamp, :player_offset, :listener_count, :description]
      argument :feed_id, :string, allow_nil?: false


      change manage_relationship(:feed_id, :feed, type: :append)
    end
  end

  calculations do
    calculate :uuid, :string, Orcasite.Radio.Calculations.DecodeUUID
  end

  relationships do
    belongs_to :candidate, Candidate
    belongs_to :feed, Feed
  end

  graphql do
    type :detection
    queries do
      get :detection, :read
      list :detections, :index
    end

    mutations do
      create :submit_detection, :submit_detection
    end
  end
end
