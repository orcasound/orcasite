defmodule OrcasiteWeb.Schema do
  use Absinthe.Schema

  import_types Absinthe.Type.Custom
  import_types OrcasiteWeb.Types.{JSON, Account, Feed}

  alias OrcasiteWeb.Resolvers

  query do
    field :feeds, list_of(:feed), resolve: &Resolvers.Feed.index/2
  end

end
