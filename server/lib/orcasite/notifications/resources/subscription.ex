defmodule Orcasite.Notifications.Subscription do
  use Ash.Resource,
    extensions: [AshAdmin.Resource],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Notifications.{Event, Notification, SubscriptionNotification, Subscriber}

  resource do
    description """
    A subscription - relates a subscriber to a notification type and a channel.
    (i.e. subscribing to :new_detection via :email)
    """
  end

  actions do
    defaults [:create, :read, :update, :destroy]

    create :email_subscribe do
      description "Create a subscription for an individual email"
      accept [:event_type, :subscriber, :email, :name]
      argument :email, :string
      argument :name, :string

      argument :event_type, :atom do
        constraints one_of: Event.types()
        default nil
      end

      argument :subscriber, :uuid

      change set_attribute(:event_type, arg(:event_type))

      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.change_attribute(:meta, %{
          name: Ash.Changeset.get_argument(changeset, :name),
          email: Ash.Changeset.get_argument(changeset, :email),
          channel: :email
        })
      end

      change manage_relationship(:subscriber, type: :append)
    end
  end

  postgres do
    table "subscriptions"
    repo Orcasite.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string
    attribute :meta, :map

    attribute :event_type, :atom do
      constraints one_of: Event.types()
    end

    attribute :last_notified_at, :utc_datetime_usec

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :subscriber, Subscriber
    has_many :subscription_notifications, SubscriptionNotification

    many_to_many :notifications, Notification do
      through SubscriptionNotification
      source_attribute_on_join_resource :subscription_id
      destination_attribute_on_join_resource :notification_id
    end

    belongs_to :last_notification, Notification
  end
end
