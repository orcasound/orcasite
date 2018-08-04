defmodule OrcasiteWeb.Schema do
  use Absinthe.Schema

  import_types Absinthe.Type.Custom
  import_types OrcasiteWeb.Types.{JSON, Account, Feed, Detection}

  alias OrcasiteWeb.Resolvers

  query do
    @desc "Get a list of feeds"
    field :feeds, list_of(:feed), resolve: &Resolvers.Feed.index/2

    @desc "Get a feed"
    field :feed, :feed do
      arg :slug, :string

      resolve &Resolvers.Feed.show/2
    end
  end

  mutation do
    @desc "Submit an orca sound detection"
    field :submit_detection, :detection do
      resolve &Resolvers.Detection.submit/3
    end
  end

end
