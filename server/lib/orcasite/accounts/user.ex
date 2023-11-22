defmodule Orcasite.Accounts.User do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication, AshAdmin.Resource, AshGraphql.Resource]

  postgres do
    table "users"
    repo Orcasite.Repo
  end

  identities do
    identity :unique_email, [:email]
  end

  attributes do
    uuid_primary_key :id
    attribute :email, :ci_string, allow_nil?: false
    attribute :hashed_password, :string, allow_nil?: false, sensitive?: true
    attribute :first_name, :string
    attribute :last_name, :string
    attribute :admin, :boolean

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  authentication do
    api Orcasite.Accounts

    strategies do
      password :password do
        identity_field :email
        sign_in_tokens_enabled? true

        register_action_accept [:first_name, :last_name]

        resettable do
          sender fn user, token, opts ->
            Task.Supervisor.async_nolink(Orcasite.TaskSupervisor, fn ->
              IO.inspect("Sending password reset email to #{user.email}")
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

  actions do
    defaults [:read, :create, :update, :destroy]

    read :by_email do
      get_by :email
    end

    read :current_user do
      get? true
      manual Orcasite.Accounts.Actions.CurrentUserRead
    end
  end

  code_interface do
    define_for Orcasite.Accounts

    define :register_with_password
    define :sign_in_with_password
    define :by_email, args: [:email]
    define :request_password_reset_with_password
    define :password_reset_with_password
  end

  admin do
    table_columns [:id, :email, :first_name, :last_name, :admin, :inserted_at]
  end

  graphql do
    type :user
    hide_fields [:hashed_password]

    queries do
      read_one :current_user, :current_user
    end

    mutations do
      create :register_with_password, :register_with_password
    end
  end

  # policies do
  #   bypass AshAuthentication.Checks.AshAuthenticationInteraction do
  #     authorize_if always()
  #   end
  # end
end
