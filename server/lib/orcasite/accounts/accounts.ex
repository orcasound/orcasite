defmodule Orcasite.Accounts do
  use Ash.Api

  resources do
    registry Orcasite.Accounts.Registry
  end
end
