defmodule OrcasiteWeb.Schema do
  use Absinthe.Schema

  import_types(Absinthe.Type.Custom)
  import_types(OrcasiteWeb.Types.{JSON, Account, Feed, Detection, Pagination})

  alias OrcasiteWeb.Resolvers

  query do
    @desc "Get logged in user"
    field(:current_user, :user, resolve: &Resolvers.Accounts.current_user/2)

    @desc "Get a list of feeds"
    field(:feeds, list_of(:feed), resolve: &Resolvers.Feed.index/2)

    @desc "Get a feed"
    field :feed, :feed do
      arg(:slug, :string)

      resolve(&Resolvers.Feed.show/2)
    end

    field :detections, list_of(:detection) do
      # TODO: Add auth inside of Radio logic
      resolve(&Resolvers.Detection.index/2)
    end

    field :candidates, :candidates do
      arg(:pagination, :pagination)
      # TODO: Add auth inside of Radio logic
      resolve(&Resolvers.Detection.list_candidates/2)
    end
  end

  mutation do
    @desc "Create user"
    field :signup, :user do
      arg(:first_name, :string)
      arg(:last_name, :string)
      arg(:email, non_null(:string))
      arg(:password, non_null(:string))

      resolve(&Resolvers.Accounts.create_user/2)
    end

    @desc "Submit an orca sound detection"
    field :submit_detection, :detection_with_lockout do
      arg(:feed_id, :id)
      arg(:playlist_timestamp, :string)
      arg(:player_offset, :decimal)
      arg(:listener_count, :integer)

      resolve(&Resolvers.Detection.create/2)
    end

    @desc "Log in"
    field :login, :user do
      arg(:email, non_null(:string))
      arg(:password, non_null(:string))

      resolve(&Resolvers.Accounts.login/2)
    end

    @desc "Log out"
    field :logout, :user do
      arg(:id)
      resolve(&Resolvers.Accounts.logout/2)
    end
  end
end
