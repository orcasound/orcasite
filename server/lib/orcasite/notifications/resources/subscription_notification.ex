defmodule Orcasite.Notifications.SubscriptionNotification do
  use Ash.Resource,
    extensions: [AshAdmin.Resource],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Notifications.{Notification, Subscription}

  resource do
    description """
    A subscription notification object. This tracks the status of a notification
    and triggers the sending of the notification.
    """
  end

  postgres do
    table "subscription_notifications"
    repo Orcasite.Repo
  end

  actions do
    defaults [:create, :read, :update, :destroy]
  end

  attributes do
    uuid_primary_key :id

    attribute :meta, :map
    attribute :channel, :atom do
      constraints one_of: [:email, :newsletter]
    end
    attribute :processed_at, :utc_datetime_usec
    attribute :status, :atom do
      constraints one_of: [:unsent, :pending, :sent, :failed]
      default :unsent
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :subscription, Subscription
    belongs_to :notification, Notification
  end
end
