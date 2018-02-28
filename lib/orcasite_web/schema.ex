defmodule OrcasiteWeb.Schema do
  use Absinthe.Schema
  import_types Absinthe.Type.Custom
  import_types OrcasiteWeb.Schema.AccountTypes
  alias OrcasiteWeb.Resolvers

  query do

    @desc "Get a node"
    field :user, :user do
      arg :id, non_null(:id)
      resolve &Resolvers.Accounts.find_user/3
    end

  end

end