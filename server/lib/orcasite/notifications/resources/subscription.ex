defmodule Orcasite.Notifications.Subscription do
  use Ash.Resource,
  extensions: [AshAdmin.Resource],
  data_layer: AshPostgres.DataLayer

  alias Orcasite.Notifications.{Event, Notification, SubscriptionNotification, Subscriber}

  resource do
    description """
    A subscription - relates a subscriber to a notification type and a channel. (i.e. subscribing to :new_candidates via :email)
    """
  end

  actions do
    defaults [:create, :read, :update, :destroy]
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
  end
end
