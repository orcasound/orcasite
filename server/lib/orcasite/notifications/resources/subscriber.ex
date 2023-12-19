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
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string

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
      filter expr(fragment("lower(meta->>'email') = lower(?)", ^arg(:email)))
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
      argument :email, :string
      argument :response_data, :map

      # TODO: Remove this argument once we deploy to production.
      argument :active_subscription, :boolean, default: true

      change set_attribute(:name, arg(:name))
      change set_attribute(:subscriber_type, :individual)

      change fn changeset, _context ->
        meta =
          case Orcasite.Accounts.User.by_email(Ash.Changeset.get_argument(changeset, :email)) do
            {:ok, %{id: user_id}} -> %{user_id: user_id}
            _ -> %{}
          end
          |> Map.put(:email, Ash.Changeset.get_argument(changeset, :email))
          |> Map.put(:response_data, Ash.Changeset.get_argument(changeset, :response_data))

        changeset
        |> Ash.Changeset.change_attribute(:meta, meta)
        |> Ash.Changeset.manage_relationship(
          :subscriptions,
          %{
            name: Ash.Changeset.get_argument(changeset, :name),
            event_type: "confirmed_candidate",
            meta: %{
              email: Ash.Changeset.get_argument(changeset, :email),
              name: Ash.Changeset.get_argument(changeset, :name),
              channel: :email
            },
            # TODO: Remove this active field once we deploy to production.
            active: Ash.Changeset.get_argument(changeset, :active_subscription)
          },
          type: :create
        )
      end
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

    format_fields meta: {Jason, :encode!, []}

    form do
      field :event_type, type: :default
    end
  end

  validations do
    validate fn changeset ->
      # Check if email subscriber already exists
      with email when is_binary(email) <- changeset |> Ash.Changeset.get_argument(:email),
           %{action_type: :create} <- changeset,
           {:get, {:error, _}} <- {:get, Orcasite.Notifications.Subscriber.by_email(email)} do
        :ok
      else
        {:get, other} ->
          {:error, [field: :email, message: "email already exists"]}

        err ->
          :ok
      end
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
