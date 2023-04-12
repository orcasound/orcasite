defmodule Orcasite.Notifications.Notification do
  use Ash.Resource,
    extensions: [AshAdmin.Resource],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Notifications.{Event, SubscriptionNotification, Subscription}

  resource do
    description """
    A notification object. Once created, this will be sent to any number of subscribers (api clients,
    newsletters, individuals, admins, etc) through their subscribed channels for the given event type
    (new candidate created, detection verified, etc).
    """
  end

  actions do
    defaults [:create, :read, :update, :destroy]
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
    has_many :subscription_notifications, SubscriptionNotification
    many_to_many :subscriptions, Subscription do
      through SubscriptionNotification
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
