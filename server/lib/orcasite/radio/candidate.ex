defmodule Orcasite.Radio.Candidate do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource],
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

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      prepare build(load: [:uuid])
    end

    create :create do
      primary? true

      argument :detections, {:array, :map}
      argument :feed, :map

      change manage_relationship(:feed, type: :append)
      change manage_relationship(:detections, type: :append)
    end

    read :find_nearby_candidate do
      get? true
      argument :timestamp, :utc_datetime
      argument :within_minutes, :integer, default: 3
      argument :feed_id, :string

      prepare fn query, _context ->
        require Ash.Query
        timestamp = Ash.Query.get_argument(query, :timestamp)
        within_minutes = Ash.Query.get_argument(query, :within_minutes)

        feed_id =
          Ash.Query.get_argument(query, :feed_id)
          |> AshUUID.identify_format()
          |> case do
            :raw ->
              Ash.Query.get_argument(query, :feed_id)

            _ ->
              "feed_" <> id = Ash.Query.get_argument(query, :feed_id)
              {:ok, feed_id} = AshUUID.Encoder.decode(id)
              feed_id
          end

        min_time = DateTime.add(timestamp, -within_minutes, :minute)
        max_time = DateTime.add(timestamp, within_minutes, :minute)

        query
        |> Ash.Query.filter(
          feed_id == ^feed_id and ^max_time >= min_time and max_time >= ^min_time
        )
      end
    end
  end

  calculations do
    calculate :uuid, :string, {Orcasite.Radio.Calculations.DecodeUUID, []}
  end

  relationships do
    has_many :detections, Detection

    belongs_to :feed, Feed
  end

  admin do
    table_columns [:id, :detection_count, :feed, :min_time, :max_time, :inserted_at]
  end

  graphql do
    type :candidate

    queries do
      get :candidate, :read
      list :candidates, :index
    end
  end
end
