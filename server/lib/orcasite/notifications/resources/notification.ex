defmodule Orcasite.Notifications.Notification do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  alias Orcasite.Notifications.{Event, NotificationInstance, Subscription}

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
    attribute :active, :boolean, default: true

    attribute :event_type, :atom do
      constraints one_of: Event.types()
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
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
    end
  end

  actions do
    defaults [:create, :read, :update, :destroy]

    read :since_notification do
      description "Get all notifications after a given notification ID."
      argument :notification_id, :uuid

      manual Orcasite.Notifications.ManualReadNotificationsSince
    end

    read :for_candidate do
      argument :candidate_id, :string, allow_nil?: false

      argument :event_type, :atom do
        constraints one_of: Event.types()
      end

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

    update :cancel_notification do
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

    create :notify_confirmed_candidate do
      description "Create a notification for confirmed candidate (i.e. detection group)"
      accept [:candidate_id, :message]
      argument :candidate_id, :uuid, allow_nil?: false

      argument :message, :string do
        description """
        What primary message subscribers will get (e.g. 'Southern Resident Killer Whales calls
        and clicks can be heard at Orcasound Lab!')
        """

        allow_nil? false
      end

      change set_attribute(:event_type, :confirmed_candidate)

      change fn changeset, _context ->
        candidate_id = Ash.Changeset.get_argument(changeset, :candidate_id)

        candidate =
          Orcasite.Radio.Candidate
          |> Orcasite.Radio.get(candidate_id)
          |> Orcasite.Radio.load!(:feed)

        changeset
        |> Ash.Changeset.change_attribute(:meta, %{
          candidate_id: candidate_id,
          node: candidate.feed.slug,
          message: Ash.Changeset.get_argument(changeset, :message)
        })
      end
    end

    create :notify_new_detection do
      description "Create a notification for a new detection (e.g. button push from user)."
      accept [:detection_id]
      argument :detection_id, :string
      argument :node, :string, allow_nil?: false
      argument :description, :string, allow_nil?: true
      argument :listener_count, :integer, allow_nil?: true
      argument :candidate_id, :string, allow_nil?: true

      change set_attribute(:event_type, :new_detection)

      change fn changeset, _context ->
        # TODO: Get node, timestamp from detection
        changeset
        |> Ash.Changeset.change_attribute(:meta, %{
          detection_id: Ash.Changeset.get_argument(changeset, :detection_id),
          node: Ash.Changeset.get_argument(changeset, :node),
          description: Ash.Changeset.get_argument(changeset, :description),
          listener_count: Ash.Changeset.get_argument(changeset, :listener_count),
          candidate_id: Ash.Changeset.get_argument(changeset, :candidate_id)
        })
      end
    end
  end

  code_interface do
    define_for Orcasite.Notifications

    define :notify_new_detection,
      action: :notify_new_detection,
      args: [:detection_id, :node, :description, :listener_count, :candidate_id]

    define :notify_confirmed_candidate,
      action: :notify_confirmed_candidate,
      args: [:candidate_id]
  end

  resource do
    description """
    Notification for a specific event type. Once created, all Subscriptions that match this Notification's
    event type (new detection, confirmed candidate, etc.) will be notified using the Subscription's particular
    channel settings (email, browser notification, webhooks).
    """
  end

  admin do
    table_columns [:id, :meta, :event_type, :inserted_at]

    format_fields meta: {Jason, :encode!, []}

    form do
      field :event_type, type: :default
    end
  end

  changes do
    change fn changeset, _context ->
             changeset
             |> Ash.Changeset.after_action(fn _, notification ->
               Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
                 Orcasite.Notifications.Subscription
                 |> Ash.Query.for_read(:available_for_notification, %{
                   notification_id: notification.id,
                   event_type: notification.event_type
                 })
                 |> Orcasite.Notifications.stream!()
                 |> Stream.map(fn subscription ->
                   Orcasite.Notifications.NotificationInstance
                   |> Ash.Changeset.for_create(:create_with_relationships, %{
                     notification: notification.id,
                     subscription: subscription.id
                   })
                   |> Orcasite.Notifications.create!()
                 end)
                 |> Stream.run()
               end)

               {:ok, notification}
             end)
           end,
           on: :create
  end

  graphql do
    type :notification

    queries do
      list :notifications_for_candidate, :for_candidate
    end

    mutations do
      create :notify_confirmed_candidate, :notify_confirmed_candidate
    end
  end
end
