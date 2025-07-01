defmodule Orcasite.Accounts.Token do
  use Ash.Resource,
    domain: Orcasite.Accounts,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication.TokenResource],
    authorizers: [Ash.Policy.Authorizer]

  postgres do
    table "tokens"
    repo Orcasite.Repo
  end

  token do
    domain Orcasite.Accounts
  end

  policies do
    bypass AshAuthentication.Checks.AshAuthenticationInteraction do
      authorize_if always()
    end
  end
end
