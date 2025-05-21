defmodule OrcasiteWeb.Schema do
  use Absinthe.Schema

  @domains [Orcasite.Radio, Orcasite.Accounts, Orcasite.Notifications]

  use AshGraphql, domains: @domains

  import_types Absinthe.Type.Custom
  import_types OrcasiteWeb.Graphql.Types.Accounts


  query do
  end

  mutation do
    import_fields :accounts_user_mutations
  end

  subscription do
  end
end
