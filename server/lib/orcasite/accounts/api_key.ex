defmodule Orcasite.Accounts.ApiKey do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Accounts,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  postgres do
    table "api_keys"
    repo Orcasite.Repo
  end

  identities do
    identity :unique_api_key, [:api_key_hash]
  end

  attributes do
    uuid_primary_key :id

    attribute :api_key_hash, :binary do
      allow_nil? false
      sensitive? true
    end

    attribute :disabled_at, :utc_datetime_usec
    attribute :expires_at, :utc_datetime_usec
  end

  code_interface do
    define :create
  end

  calculations do
    calculate :invalid,
              :boolean,
              expr(
                (not is_nil(expires_at) and expires_at < now()) or
                  (not is_nil(disabled_at) and disabled_at < now())
              )

    calculate :valid, :boolean, expr(not invalid)
  end

  relationships do
    belongs_to :user, Orcasite.Accounts.User
  end

  policies do
    bypass AshAuthentication.Checks.AshAuthenticationInteraction do
      authorize_if always()
    end
  end

  actions do
    defaults [:read, :destroy]

    update :update do
      primary? true
      accept [:disabled_at]
      validate attributes_absent(:disabled_at)
    end

    create :create do
      primary? true
      accept [:user_id, :expires_at]

      change {AshAuthentication.Strategy.ApiKey.GenerateApiKey,
              prefix: :orcasound, hash: :api_key_hash}
    end
  end
end
