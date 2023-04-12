defmodule Orcasite.Notifications.SubscriptionNotification do
  use Ash.Resource,
    extensions: [AshAdmin.Resource],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Notifications.{Notification, Subscription}

  resource do
    description """
    A subscription notification object. This is a join table between subscriptions and notifications.
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

    attribute :processed_at, :utc_datetime_usec

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :subscription, Subscription
    belongs_to :notification, Notification
  end
end
