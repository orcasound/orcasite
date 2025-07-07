defmodule Orcasite.Accounts do
  use Ash.Domain, extensions: [AshAdmin.Domain, AshGraphql.Domain]

  resources do
    resource Orcasite.Accounts.User
    resource Orcasite.Accounts.Token
    resource Orcasite.Accounts.ApiKey
  end

  admin do
    show? true
  end

  graphql do
  end
end
