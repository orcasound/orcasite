defmodule Orcasite.Notifications.Token do
  use Ash.Resource,
    domain: Orcasite.Notifications,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication.TokenResource],
    authorizers: Ash.Policy.Authorizer

  token do
    domain Orcasite.Notifications
  end

  postgres do
    table "subscriber_tokens"
    repo Orcasite.Repo
  end

  policies do
    bypass AshAuthentication.Checks.AshAuthenticationInteraction do
      authorize_if always()
    end

    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end
  end
end
