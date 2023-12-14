defmodule OrcasiteWeb.Schema do
  use Absinthe.Schema

  @apis [Orcasite.Radio, Orcasite.Accounts, Orcasite.Notifications]

  use AshGraphql, apis: @apis

  import_types Absinthe.Type.Custom
  import_types OrcasiteWeb.Graphql.Types.Accounts


  query do
  end

  mutation do
    import_fields :accounts_user_mutations
  end
end
