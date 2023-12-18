defmodule Orcasite.Accounts do
  use Ash.Api, extensions: [AshAdmin.Api, AshGraphql.Api]

  resources do
    registry Orcasite.Accounts.Registry
  end

  admin do
    show? true
  end

  graphql do
  end
end
