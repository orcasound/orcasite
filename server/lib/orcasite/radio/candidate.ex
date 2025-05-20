defmodule Orcasite.Radio.Candidate do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  alias Orcasite.Radio.{Detection, Feed}

  postgres do
    table "candidates"
    repo Orcasite.Repo

    custom_indexes do
      index [:min_time]
      index [:max_time]
      index [:inserted_at]
      index [:category]
    end

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"
  end

  attributes do
    uuid_attribute :id, prefix: "cand", public?: true

    attribute :detection_count, :integer, public?: true
    attribute :min_time, :utc_datetime_usec, allow_nil?: false, public?: true
    attribute :max_time, :utc_datetime_usec, allow_nil?: false, public?: true
    attribute :visible, :boolean, default: true, public?: true

    attribute :category, Orcasite.Types.DetectionCategory, public?: true

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  calculations do
    calculate :uuid, :string, {Orcasite.Radio.Calculations.DecodeUUID, []}

    calculate :audio_category,
              Orcasite.Types.AudioCategory,
              expr(
                cond do
                  category == :whale -> :biophony
                  category == :vessel -> :anthrophony
                  category == :other -> :geophony
                end
              ) do
      public? true
    end
  end

  relationships do
    has_many :detections, Detection do
      public? true
    end

    belongs_to :feed, Feed do
      allow_nil? false
      public? true
    end
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass action_type(:read) do
      authorize_if always()
    end

    bypass action_type(:create) do
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
    defaults [:read, :destroy]

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

      accept [:min_time, :max_time, :detection_count, :category]

      argument :detections, {:array, :map}
      argument :feed, :map

      change manage_relationship(:feed, type: :append)
      change manage_relationship(:detections, type: :append)
    end

    read :find_nearby_candidate do
      get? true

      argument :category, Orcasite.Types.DetectionCategory do
        allow_nil? false
      end

      argument :timestamp, :utc_datetime
      argument :within_minutes, :integer, default: 3
      argument :feed_id, :string

      prepare fn query, _context ->
        require Ash.Query
        timestamp = Ash.Query.get_argument(query, :timestamp)
        within_minutes = Ash.Query.get_argument(query, :within_minutes)
        category = Ash.Query.get_argument(query, :category)

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
          feed_id == ^feed_id and ^max_time >= min_time and max_time >= ^min_time and
            category == ^category
        )
      end
    end

    update :update do
      primary? true
      accept [:min_time, :max_time, :detection_count, :visible, :category]
    end

    update :cancel_notifications do
      accept []
      require_atomic? false

      argument :event_type, Orcasite.Types.NotificationEventType do
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
          |> Ash.read!(authorize?: false)
          |> Enum.map(fn notification ->
            notification
            |> Ash.Changeset.for_update(:cancel_notification, %{})
            |> Ash.update!(authorize?: false)
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
    attribute_types feed_id: :id

    queries do
      get :candidate, :read
      list :candidates, :index
    end

    mutations do
      update :cancel_candidate_notifications, :cancel_notifications
    end
  end
end
