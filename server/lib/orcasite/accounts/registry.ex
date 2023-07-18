defmodule Orcasite.Accounts.Registry do
  use Ash.Registry, extensions: [Ash.Registry.ResourceValidations]

  entries do
    entry Orcasite.Accounts.User
    entry Orcasite.Accounts.Token
  end
end
