defmodule Orcasite.Notifications do
  use Ash.Api, extensions: [AshAdmin.Api, AshJsonApi.Api, AshGraphql.Api]

  resources do
    registry Orcasite.Notifications.Registry
  end

  admin do
    show? true
  end

  json_api do
    log_errors? true
  end

  graphql do
  end
end
