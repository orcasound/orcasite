defmodule Orcasite.Radio do
  use Ash.Api, extensions: [AshAdmin.Api, AshGraphql.Api]

  resources do
    registry Orcasite.Radio.Registry
  end

  admin do
    show? true
  end

  graphql do
    authorize? false
  end
end
