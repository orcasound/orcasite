defmodule Orcasite.Accounts.User do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication]


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
      end
    end

    tokens do
      enabled? true
      token_resource Orcasite.Accounts.Token
      signing_secret fn _, _ ->
        {:ok, Application.get_env(:orcasite, OrcasiteWeb.Endpoint)[:secret_key_base]}
      end
    end
  end

  postgres do
    table "users"
    repo Orcasite.Repo
  end

  identities do
    identity :unique_email, [:email]
  end

  actions do
    defaults [:read, :create, :update, :destroy]
  end
end
