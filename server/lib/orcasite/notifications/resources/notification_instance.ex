defmodule Orcasite.Notifications.NotificationInstance do
  use Ash.Resource,
  domain: Orcasite.Notifications,
    extensions: [AshAdmin.Resource],
    data_layer: Ash.DataLayer.Ets

  alias Orcasite.Notifications.Changes.ExtractNotificationInstanceMeta
  alias Orcasite.Notifications.{Notification, Subscription}

  attributes do
    uuid_primary_key :id

    attribute :meta, :map, default: %{}

    attribute :channel, :atom do
      constraints one_of: [:email]
    end

    attribute :processed_at, :utc_datetime_usec

    attribute :status, :atom do
      constraints one_of: [:new, :pending, :sent, :failed]
      default :new
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :subscription, Subscription
    belongs_to :notification, Notification
  end

  code_interface do
    define :update, action: :update, args: [:status, :meta, :processed_at]
  end

  resource do
    description """
    Sends a single notification to a subscription
    """
  end

  actions do
    defaults [:create, :read, :update, :destroy]

    create :create_with_relationships do
      argument :subscription, :uuid
      argument :notification, :uuid

      change manage_relationship(:subscription, type: :append)
      change manage_relationship(:notification, type: :append)

      change {ExtractNotificationInstanceMeta, []}

      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.after_action(fn _, record ->
          %{
            notification_instance_id: record.id,
            notification_id: record.notification_id,
            subscription_id: record.subscription_id,
            meta: record.meta
          }
          |> Orcasite.Notifications.Workers.SendNotificationEmail.new()
          |> Oban.insert()

          {:ok, record}
        end)
      end
    end
  end
end
