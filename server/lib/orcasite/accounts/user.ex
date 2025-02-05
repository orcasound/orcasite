defmodule Orcasite.Accounts.User do
  use Ash.Resource,
    domain: Orcasite.Accounts,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication, AshAdmin.Resource, AshGraphql.Resource],
    authorizers: [Ash.Policy.Authorizer]

  postgres do
    table "users"
    repo Orcasite.Repo
  end

  identities do
    identity :unique_email, [:email]
    identity :unique_username, [:username]
  end

  attributes do
    uuid_primary_key :id
    attribute :email, :ci_string, allow_nil?: false, public?: true
    attribute :hashed_password, :string, allow_nil?: false, sensitive?: true
    attribute :first_name, :string, public?: true
    attribute :last_name, :string, public?: true
    attribute :admin, :boolean, default: false, allow_nil?: false, public?: true
    attribute :moderator, :boolean, default: false, allow_nil?: false, public?: true

    attribute :username, :string do
      public? true
      allow_nil? true
      constraints allow_empty?: false, trim?: true
    end

    # Profile fields
    attribute :is_scientist, :boolean, default: false, allow_nil?: false, public?: true
    attribute :organization, :string, allow_nil?: true, public?: true

    # Preferences
    attribute :volunteering, :boolean, default: false, allow_nil?: false, public?: true
    attribute :user_testing, :boolean, default: false, allow_nil?: false, public?: true
    attribute :newsletter, :boolean, default: false, allow_nil?: false, public?: true

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  authentication do
    domain Orcasite.Accounts

    strategies do
      password :password do
        identity_field :email
        sign_in_tokens_enabled? true

        register_action_accept [:first_name, :last_name, :username]

        resettable do
          sender fn user, token, opts ->
            Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
              Orcasite.Accounts.Email.reset_password(user, token, opts)
              |> Orcasite.Mailer.deliver()
            end)
          end
        end
      end
    end

    tokens do
      enabled? true
      token_resource Orcasite.Accounts.Token
      signing_secret Orcasite.Accounts.Secrets
    end

    select_for_senders [:id, :email, :first_name, :last_name]
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass AshAuthentication.Checks.AshAuthenticationInteraction do
      authorize_if always()
    end
    
    policy action_type(:read) do
      authorize_if expr(id == ^actor(:id))
    end

    policy action(:current_user) do
      authorize_if always()
    end

    bypass action(:register_with_password) do
      authorize_if always()
    end

    bypass action(:sign_in_with_password) do
      authorize_if always()
    end

    bypass action(:request_password_reset_with_password) do
      authorize_if always()
    end

    bypass action(:password_reset_with_password) do
      authorize_if always()
    end

    policy action(:update_profile) do
      authorize_if expr(id == ^actor(:id))
    end

    policy action(:update_preferences) do
      authorize_if expr(id == ^actor(:id))
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]

    read :by_email do
      get_by :email
    end

    read :current_user do
      get? true
      manual Orcasite.Accounts.Actions.CurrentUserRead
    end

    update :update_profile do
      accept [
        :username,
        :first_name,
        :last_name,
        :is_scientist,
        :organization
      ]
    end

    update :update_preferences do
      argument :live_notifications, :boolean, default: false, allow_nil?: false
      accept [
        :volunteering,
        :user_testing,
        :newsletter
      ]

      # TODO: When live_notifications is true, create a subscription for the user
    end
  end

  code_interface do
    domain Orcasite.Accounts

    define :register_with_password
    define :sign_in_with_password
    define :by_email, args: [:email]
    define :request_password_reset_with_password
    define :password_reset_with_password
    define :update_profile
    define :update_preferences
  end

  admin do
    table_columns [
      :id,
      :username,
      :email,
      :first_name,
      :last_name,
      :admin,
      :moderator,
      :inserted_at
    ]

    actor? true
    read_actions [:read, :current_user]
  end

  graphql do
    type :user

    queries do
      read_one :current_user, :current_user
    end

    mutations do
      create :register_with_password, :register_with_password do
        modify_resolution {__MODULE__, :after_registration, []}
      end
      update :update_user_profile, :update_profile
      update :update_user_preferences, :update_preferences
    end
  end

  def after_registration(resolution, _query, result) do
    result
    |> case do
      {:ok, user} ->
        resolution
        |> Map.update!(:context, fn ctx ->
          Map.put(ctx, :current_user, user)
        end)
      _ ->
        resolution
    end
  end
end
