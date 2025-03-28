defmodule Orcasite.Notifications.Subscription do
  use Ash.Resource,
    domain: Orcasite.Notifications,
    extensions: [AshAuthentication, AshAdmin.Resource],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Notifications.{Notification, NotificationInstance, Subscriber}

  postgres do
    table "subscriptions"
    repo Orcasite.Repo

    custom_indexes do
      index [:meta], using: "gin"
    end
  end

  identities do
    # Needed by magic_token. Primary key doesn't show up as an identity otherwise
    identity :id, [:id]
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string, public?: true
    attribute :meta, :map, default: %{}, public?: true

    attribute :active, :boolean, default: true, public?: true

    attribute :event_type, Orcasite.Types.NotificationEventType, public?: true

    attribute :last_notified_at, :utc_datetime_usec

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :subscriber, Subscriber
    has_many :notification_instances, NotificationInstance

    many_to_many :notifications, Notification do
      through NotificationInstance
      source_attribute_on_join_resource :subscription_id
      destination_attribute_on_join_resource :notification_id
    end

    belongs_to :last_notification, Notification
  end

  code_interface do
    define :update_last_notification,
      action: :update_last_notification,
      args: [:last_notification]

    define :available_for_notification,
      action: :available_for_notification,
      args: [:notification_id, :event_type, {:optional, :minutes_ago}]
  end

  resource do
    description """
    A subscription - relates a subscriber to a notification type and a channel.
    (i.e. subscribing to :new_detection via :email)
    """
  end

  authentication do
    domain Orcasite.Notifications

    strategies do
      magic_link :unsubscribe do
        identity_field :id

        single_use_token? false
        # 14 days (in minutes)
        token_lifetime 40_320

        sender fn _subscription, _token, _opts ->
          # IO.inspect({subscription, token},
          #   label:
          #     "{subscription, token} (server/lib/orcasite/notifications/resources/subscription.ex:#{__ENV__.line})"
          # )

          # Orcasite.Emails.deliver_magic_link(user, token)
          :ok
        end
      end
    end

    tokens do
      enabled? true
      token_resource Orcasite.Notifications.Token

      signing_secret fn _, _ ->
        {:ok, Application.get_env(:orcasite, OrcasiteWeb.Endpoint)[:secret_key_base]}
      end
    end
  end

  actions do
    defaults [:destroy, :read, create: :*]

    read :by_email do
      argument :email, :string
      filter expr(meta[:email] == ^arg(:email))
    end

    read :available_for_notification do
      description """
      Subscriptions that can be sent a notification. Finds subscriptions that haven't been sent a
      notification recently and that match the notification's event type.
      """

      pagination keyset?: true, required?: false

      argument :notification_id, :uuid

      argument :event_type, Orcasite.Types.NotificationEventType do
        default nil
      end

      argument :minutes_ago, :integer, default: 5

      filter expr(
               active and
                 event_type == ^arg(:event_type) and
                 (is_nil(last_notification_id) or last_notification_id != ^arg(:notification_id)) and
                 (is_nil(last_notified_at) or
                    fragment(
                      "? < timezone('UTC', now()) - ?::numeric * interval '1 minute'",
                      last_notified_at,
                      ^arg(:minutes_ago)
                    ))
             )
    end

    create :email_subscribe do
      description "Create a subscription for an individual email"

      argument :name, :string
      argument :email, :string

      argument :event_type, Orcasite.Types.NotificationEventType do
        default nil
      end

      argument :subscriber, :uuid

      change set_attribute(:event_type, arg(:event_type))
      change set_attribute(:name, arg(:name))

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

    update :update do
      primary? true
      require_atomic? false

      accept [:active]
      argument :subscriber_id, :uuid

      change manage_relationship(:subscriber_id, :subscriber, type: :append)
    end

    update :update_last_notification do
      require_atomic? false

      argument :last_notification, :uuid

      change manage_relationship(:last_notification, type: :append)
      change set_attribute(:last_notified_at, &DateTime.utc_now/0)
    end

    update :unsubscribe do
      change set_attribute(:active, false)
    end
  end

  def unsubscribe_token(subscription) do
    strategy = AshAuthentication.Info.strategy!(Orcasite.Notifications.Subscription, :unsubscribe)
    {:ok, token} = AshAuthentication.Strategy.MagicLink.request_token_for(strategy, subscription)
    token
  end

  admin do
    table_columns [:id, :name, :meta, :active, :event_type, :subscriber_id, :inserted_at]
    read_actions [:read, :available_for_notification, :by_email]
    format_fields meta: {Jason, :encode!, []}
  end
end
