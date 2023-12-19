defmodule Orcasite.Radio.Candidate do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  alias Orcasite.Radio.{Detection, Feed}
  alias Orcasite.Notifications.Event

  postgres do
    table "candidates"
    repo Orcasite.Repo

    custom_indexes do
      index [:min_time]
      index [:max_time]
      index [:inserted_at]
    end

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"
  end

  attributes do
    uuid_attribute(:id, prefix: "cand")

    attribute :detection_count, :integer
    attribute :min_time, :utc_datetime_usec, allow_nil?: false
    attribute :max_time, :utc_datetime_usec, allow_nil?: false
    attribute :visible, :boolean, default: true

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  calculations do
    calculate :uuid, :string, {Orcasite.Radio.Calculations.DecodeUUID, []}
  end

  relationships do
    has_many :detections, Detection

    belongs_to :feed, Feed, allow_nil?: false
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    policy action_type(:read) do
      authorize_if always()
    end

    policy action_type(:create) do
      authorize_if always()
    end

    policy changing_attributes([:visible]) do
      authorize_if actor_attribute_equals(:moderator, true)
    end

    policy expr(is_nil(visible) or not visible) do
      authorize_if actor_attribute_equals(:moderator, true)
    end
  end

  actions do
    defaults [:update, :destroy]

    read :read do
      primary? true
      prepare build(load: [:uuid], sort: [inserted_at: :desc])
    end

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      prepare build(load: [:uuid], sort: [inserted_at: :desc])
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

    update :cancel_notifications do
      accept []

      argument :event_type, :atom do
        constraints one_of: Event.types()
        default :confirmed_candidate
      end

      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.after_action(fn _, record ->
          Orcasite.Notifications.Notification
          |> Ash.Query.for_read(:for_candidate, %{
            candidate_id: record.id,
            event_type: Ash.Changeset.get_argument(changeset, :event_type),
            active: true
          })
          |> Orcasite.Notifications.read!()
          |> Enum.map(fn notification ->
            notification
            |> Ash.Changeset.for_update(:cancel_notification, %{})
            |> Orcasite.Notifications.update!()
          end)

          {:ok, record}
        end)
      end
    end
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

    mutations do
      update :cancel_candidate_notifications, :cancel_notifications
    end
  end
end
