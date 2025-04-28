defmodule Orcasite.Notifications.Notification do
  use Ash.Resource,
    domain: Orcasite.Notifications,
    extensions: [AshAdmin.Resource, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  alias Orcasite.Notifications.{NotificationInstance, Subscription}

  resource do
    description """
    Notification for a specific event type. Once created, all Subscriptions that match this Notification's
    event type (new detection, confirmed candidate, etc.) will be notified using the Subscription's particular
    channel settings (email, browser notification, webhooks).
    """
  end

  postgres do
    table "notifications"
    repo Orcasite.Repo

    custom_indexes do
      index [:meta], using: "gin"
    end
  end

  attributes do
    uuid_primary_key :id

    attribute :meta, :map, default: %{}
    attribute :active, :boolean, default: true, public?: true

    attribute :event_type, Orcasite.Types.NotificationEventType, public?: true

    attribute :target_count, :integer, public?: true
    attribute :notified_count, :integer, default: 0, public?: true
    attribute :notified_count_updated_at, :utc_datetime, public?: true

    create_timestamp :inserted_at, writable?: false, public?: true
    update_timestamp :updated_at
  end

  calculations do
    calculate :progress,
              :float,
              expr(
                if(target_count == 0 or is_nil(target_count),
                  do: 1,
                  else: notified_count / target_count
                )
              ),
              public?: true

    calculate :finished, :boolean, expr(notified_count == target_count), public?: true
  end

  relationships do
    has_many :notification_instances, NotificationInstance

    many_to_many :subscriptions, Subscription do
      through NotificationInstance
      source_attribute_on_join_resource :notification_id
      destination_attribute_on_join_resource :subscription_id
    end
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass actor_attribute_equals(:moderator, true) do
      authorize_if action(:notify_confirmed_candidate)
      authorize_if action(:cancel_notification)
      authorize_if action(:for_candidate)
      authorize_if action(:for_bout)
    end
  end

  code_interface do
    define :notify_new_detection,
      action: :notify_new_detection,
      args: [:detection, :candidate, :feed]

    define :notify_confirmed_candidate,
      action: :notify_confirmed_candidate,
      args: [:candidate_id]

    define :increment_notified_count
  end

  actions do
    defaults [:create, :read, :destroy]

    read :since_notification do
      description "Get all notifications after a given notification ID."
      argument :notification_id, :uuid

      manual Orcasite.Notifications.ManualReadNotificationsSince
    end

    read :for_bout do
      prepare build(sort: [inserted_at: :desc])
      argument :bout_id, :string, allow_nil?: false

      argument :event_type, Orcasite.Types.NotificationEventType

      argument :active, :boolean

      filter expr(
               fragment("(? @> ?)", meta, expr(%{bout_id: ^arg(:bout_id)})) and
                 if(not is_nil(^arg(:event_type)),
                   do: event_type == ^arg(:event_type),
                   else: true
                 ) and
                 if(not is_nil(^arg(:active)),
                   do: active == ^arg(:active),
                   else: true
                 )
             )

      prepare fn query, context ->
        query
      end
    end

    read :for_candidate do
      prepare build(sort: [inserted_at: :desc])
      argument :candidate_id, :string, allow_nil?: false

      argument :event_type, Orcasite.Types.NotificationEventType

      argument :active, :boolean

      filter expr(
               fragment("(? @> ?)", meta, expr(%{candidate_id: ^arg(:candidate_id)})) and
                 if(not is_nil(^arg(:event_type)),
                   do: event_type == ^arg(:event_type),
                   else: true
                 ) and
                 if(not is_nil(^arg(:active)),
                   do: active == ^arg(:active),
                   else: true
                 )
             )
    end

    update :update do
      primary? true
      accept [:target_count]
    end

    update :cancel_notification do
      require_atomic? false
      accept []
      change set_attribute(:active, false)

      change fn changeset, _context ->
        require Ecto.Query

        changeset
        |> Ash.Changeset.after_action(fn _, record ->
          Oban.Job
          |> Ecto.Query.where(worker: "Orcasite.Notifications.Workers.SendNotificationEmail")
          |> Ecto.Query.where([o], o.args["notification_id"] == ^record.id)
          |> Oban.cancel_all_jobs()

          {:ok, record}
        end)
      end
    end

    update :increment_notified_count do
      change atomic_update(:notified_count, expr(notified_count + 1))
      change atomic_update(:notified_count_updated_at, expr(now()))
    end

    create :notify_bout do
      description "Create a notification for a live bout"
      argument :bout_id, :string, allow_nil?: false

      argument :message, :string do
        description """
        What primary message subscribers will get (e.g. 'Southern Resident Killer Whales calls
        and clicks can be heard at Orcasound Lab!')
        """

        allow_nil? false
      end

      change set_attribute(:event_type, :confirmed_candidate)

      change before_action(fn changeset, _context ->
               bout_id =
                 Ash.Changeset.get_argument(changeset, :bout_id)

               bout =
                 Orcasite.Radio.Bout
                 |> Ash.get(bout_id)
                 |> Ash.load!(:feed)

               changeset
               |> Ash.Changeset.force_change_attribute(:meta, %{
                 bout_id: bout_id,
                 node: bout.feed.slug,
                 message: Ash.Changeset.get_argument(changeset, :message)
               })
             end)
    end

    create :notify_confirmed_candidate do
      description "Create a notification for confirmed candidate (i.e. detection group)"
      argument :candidate_id, :string, allow_nil?: false

      argument :message, :string do
        description """
        What primary message subscribers will get (e.g. 'Southern Resident Killer Whales calls
        and clicks can be heard at Orcasound Lab!')
        """

        allow_nil? false
      end

      change set_attribute(:event_type, :confirmed_candidate)

      change before_action(fn changeset, _context ->
               candidate_id =
                 Ash.Changeset.get_argument(changeset, :candidate_id)

               candidate =
                 Orcasite.Radio.Candidate
                 |> Ash.get(candidate_id)
                 |> Ash.load!(:feed)

               changeset
               |> Ash.Changeset.force_change_attribute(:meta, %{
                 candidate_id: candidate_id,
                 node: candidate.feed.slug,
                 message: Ash.Changeset.get_argument(changeset, :message)
               })
             end)
    end

    create :notify_live_bout do
      description "Create a notification for live bout"
      argument :bout_id, :string, allow_nil?: false

      argument :message, :string do
        description """
        What primary message subscribers will get (e.g. 'Southern Resident Killer Whales calls
        and clicks can be heard at Orcasound Lab!')
        """

        allow_nil? false
      end

      change set_attribute(:event_type, :live_bout)

      change before_action(fn changeset, _context ->
               bout_id =
                 Ash.Changeset.get_argument(changeset, :bout_id)

               bout =
                 Orcasite.Radio.Bout
                 |> Ash.get(bout_id)
                 |> Ash.load!(:feed)

               changeset
               |> Ash.Changeset.force_change_attribute(:meta, %{
                 bout_id: bout_id,
                 node: bout.feed.slug,
                 message: Ash.Changeset.get_argument(changeset, :message)
               })
             end)
    end

    create :notify_new_detection do
      description "Create a notification for a new detection (e.g. button push from user)."
      argument :detection, :map, allow_nil?: false
      argument :feed, :map, allow_nil?: false
      argument :candidate, :map, allow_nil?: false

      change set_attribute(:event_type, :new_detection)

      change fn changeset, _context ->
        # id, description, listener count
        # TODO: Get node, timestamp from detection
        detection = Ash.Changeset.get_argument(changeset, :detection)
        feed = Ash.Changeset.get_argument(changeset, :feed)

        candidate =
          Ash.Changeset.get_argument(changeset, :candidate) |> Ash.load!(:audio_category)

        changeset
        |> Ash.Changeset.change_attribute(:meta, %{
          detection_id: detection.id,
          description: detection.description,
          listener_count: detection.listener_count,
          node: feed.slug,
          candidate_id: candidate.id,
          start_time: candidate.min_time |> DateTime.to_iso8601(),
          category: candidate.audio_category
        })
      end
    end
  end

  changes do
    change fn changeset, _context ->
             changeset
             |> Ash.Changeset.after_action(fn _, %{id: notification_id} = notification ->
               Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
                 target_count =
                   Orcasite.Notifications.Subscription
                   |> Ash.Query.for_read(:available_for_notification, %{
                     notification_id: notification_id,
                     event_type: notification.event_type
                   })
                   |> Ash.stream!()
                   |> Stream.map(fn subscription ->
                     Orcasite.Notifications.NotificationInstance
                     |> Ash.Changeset.for_create(:create_with_relationships, %{
                       notification: notification,
                       subscription: subscription
                     })
                     |> Ash.create!()
                   end)
                   |> Enum.reduce(0, fn _, sum -> sum + 1 end)

                 notification
                 |> Ash.Changeset.for_update(:update, %{target_count: target_count})
                 |> Ash.update(authorize?: false)
               end)

               {:ok, notification}
             end)
           end,
           on: :create
  end

  preparations do
    prepare build(load: [:progress, :finished])
  end

  admin do
    table_columns [:id, :meta, :event_type, :inserted_at]

    format_fields meta: {Jason, :encode!, []}

    form do
      field :event_type, type: :default
    end
  end

  graphql do
    type :notification

    queries do
      list :notifications_for_candidate, :for_candidate
      list :notifications_for_bout, :for_bout
    end

    mutations do
      create :notify_confirmed_candidate, :notify_confirmed_candidate
      create :notify_live_bout, :notify_live_bout
      update :cancel_notification, :cancel_notification
    end

    subscriptions do
      pubsub OrcasiteWeb.Endpoint

      subscribe :bout_notification_sent do
        read_action :for_bout
        actions [:increment_notified_count]
      end
    end
  end
end
