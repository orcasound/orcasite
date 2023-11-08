defmodule Orcasite.Accounts.Registry do
  use Ash.Registry, extensions: [Ash.Registry.ResourceValidations]

  entries do
    entry Orcasite.Accounts.User # User resource
    entry Orcasite.Accounts.Token # Token resource
  end
end
