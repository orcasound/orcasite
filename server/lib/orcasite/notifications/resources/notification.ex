defmodule Orcasite.Notifications.Notification do
  use Ash.Resource,
    extensions: [AshAdmin.Resource],
    data_layer: AshPostgres.DataLayer,
    notifiers: [Orcasite.Notifications.Notifiers.NotifySubscriptions]

  alias Orcasite.Notifications.{Event, NotificationInstance, Subscription}

  resource do
    description """
    Notification for a specific event type. Once created, all Subscriptions that match this Notification's
    event type (new detection, confirmed candidate, etc.) will be notified using the Subscription's particular
    channel settings (email, browser notification, webhooks).
    """
  end

  actions do
    defaults [:create, :read, :update, :destroy]

    create :notify_confirmed_candidate do
      description "Create a notification for confirmed candidate (i.e. detection group)"
      accept [:candidate_id]
      argument :candidate_id, :integer
      argument :node, :string, allow_nil?: false

      change set_attribute(:event_type, :confirmed_candidate)

      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.change_attribute(:meta, %{
          candidate_id: Ash.Changeset.get_argument(changeset, :candidate_id),
          node: Ash.Changeset.get_argument(changeset, :node)
        })
      end
    end

    create :notify_new_detection do
      description "Create a notification for a new detection (e.g. button push from user)."
      accept [:detection_id]
      argument :detection_id, :integer
      argument :node, :string, allow_nil?: false

      change set_attribute(:event_type, :new_detection)

      change fn changeset, _context ->
        # TODO: Get node, timestamp from detection
        changeset
        |> Ash.Changeset.change_attribute(:meta, %{
          detection_id: Ash.Changeset.get_argument(changeset, :detection_id),
          node: Ash.Changeset.get_argument(changeset, :node)
        })
      end
    end
  end

  postgres do
    table "notifications"
    repo Orcasite.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :meta, :map

    attribute :processed_at, :utc_datetime_usec,
      description: "The time at which the notification was processed."

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

  admin do
    form do
      field :event_type, type: :default
    end
  end
end
