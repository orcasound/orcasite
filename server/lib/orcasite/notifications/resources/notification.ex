defmodule Orcasite.Notifications.Notification do
  use Ash.Resource

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

  attributes do
    uuid_primary_key :id

    attribute :meta, :map
    attribute :processed_at, :utc_datetime_usec, description: "The time at which the notification was processed."
    attribute :event_type, :string
    create_timestamp :inserted_at
    update_timestamp :updated_at
  end
end
