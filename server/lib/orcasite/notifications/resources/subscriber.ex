defmodule Orcasite.Notifications.Subscriber do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshAuthentication, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer

  alias Orcasite.Notifications.{Subscription}

  postgres do
    table "subscribers"
    repo Orcasite.Repo

    custom_indexes do
      index [:meta], using: "gin"
    end
  end

  identities do
    # Needed by magic_token. Primary key doesn't show up as an identity otherwise
    identity :id, [:id]
    identity :unique_email, [:email]
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string
    attribute :email, :ci_string

    attribute :subscriber_type, :atom do
      constraints one_of: [:individual, :organization]
    end

    attribute :meta, :map, default: %{}

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    has_many :subscriptions, Subscription
  end

  authentication do
    api Orcasite.Notifications

    strategies do
      magic_link :manage_subscriptions do
        identity_field :id

        single_use_token? false
        # 14 days (in minutes)
        token_lifetime 1_209_600

        sender fn _subscriber, _token, _opts ->
          # IO.inspect({subscriber, token},
          #   label:
          #     "subscriber/token (server/lib/orcasite/notifications/resources/subscriber.ex:#{__ENV__.line})"
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
    defaults [:create, :read, :update, :destroy]

    read :by_email do
      get? true
      argument :email, :string
      get_by :email
    end

    create :individual_subscriber do
      description "Create a subscriber for an individual"
      accept [:name, :email, :user_id]
      argument :name, :string
      argument :email, :string
      argument :user_id, :string

      change set_attribute(:name, arg(:name))
      change set_attribute(:subscriber_type, :individual)

      change fn changeset, _context ->
        changeset
        |> Ash.Changeset.change_attribute(:meta, %{
          user_id: Ash.Changeset.get_argument(changeset, :user_id),
          email: Ash.Changeset.get_argument(changeset, :email)
        })
      end
    end

    create :confirmed_candidate_subscribe do
      argument :name, :string
      argument :email, :string, allow_nil?: false
      argument :response_data, :map

      manual Orcasite.Notifications.Changes.SubscriberCreate
    end
  end

  code_interface do
    define_for Orcasite.Notifications
    define :by_email, args: [:email]
  end

  resource do
    description """
    A subscriber object. Can relate to an individual, an organization, a newsletter, or an admin.
    """
  end

  admin do
    table_columns [:id, :name, :meta, :inserted_at]
    read_actions [:read, :by_email]
    format_fields meta: {Jason, :encode!, []}

    form do
      field :event_type, type: :default
    end
  end

  json_api do
    type "subscriber"

    routes do
      base "/subscribers"

      post :confirmed_candidate_subscribe
    end
  end
end
