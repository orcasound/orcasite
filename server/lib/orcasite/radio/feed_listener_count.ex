defmodule Orcasite.Radio.FeedListenerCount do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer

  resource do
    description "Track listener counts for each feed"
  end

  postgres do
    repo Orcasite.Repo
    table "feed_listener_counts"

    custom_indexes do
      # For searching particularly active periods
      index [:count]
      index [:timestamp]
      index [:feed_id]
    end
  end

  attributes do
    uuid_v7_primary_key :id

    attribute :count, :integer, allow_nil?: false, public?: true
    attribute :timestamp, :utc_datetime, allow_nil?: false, public?: true

    timestamps()
  end

  actions do
    defaults [:read, create: :*, update: :*]

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      argument :feed_id, :string, allow_nil?: false

      filter expr(if not is_nil(^arg(:feed_id)), do: feed_id == ^arg(:feed_id), else: true)
      prepare build(load: [:uuid], sort: [inserted_at: :desc])
    end
  end

  relationships do
    belongs_to :feed, Orcasite.Radio.Feed do
      public? true
    end
  end

  graphql do
    type :listener_count
    attribute_types feed_id: :id

    queries do
      list :listener_count, :index
    end
  end

  json_api do
    type "listener_count"

    routes do
      base "/listener_counts"

      index :index
    end
  end
end
