defmodule Orcasite.Radio do
  use Ash.Api, extensions: [AshAdmin.Api, AshGraphql.Api, AshJsonApi.Api]

  resources do
    registry Orcasite.Radio.Registry
  end

  admin do
    show? true
  end

  graphql do
  end

  json_api do
    log_errors? true
  end
end
