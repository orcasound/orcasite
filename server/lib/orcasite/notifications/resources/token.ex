defmodule Orcasite.Notifications.Token do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer,
    extensions: [AshAuthentication.TokenResource, Ash.Policy.Authorizer]

  token do
    api Orcasite.Notifications
  end

  postgres do
    table "tokens"
    repo Orcasite.Repo
  end

  # policies do
  #   bypass AshAuthentication.Checks.AshAuthenticationInteraction do
  #     authorize_if always()
  #   end

  #   bypass actor_attribute_equals(:admin, true) do
  #     authorize_if always()
  #   end
  # end
end
