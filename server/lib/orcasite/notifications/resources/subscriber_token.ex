defmodule Orcasite.Notifications.SubscriberToken do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication.TokenResource]

  token do
    api Orcasite.Notifications
  end

  postgres do
    table "subscriber_tokens"
    repo Orcasite.Repo
  end

  policies do
    bypass AshAuthentication.Checks.AshAuthenticationInteraction do
      authorize_if always()
    end
  end
end
