defmodule OrcasiteWeb.Schema do
  use Absinthe.Schema

  import_types(Absinthe.Type.Custom)
  import_types(OrcasiteWeb.Types.{JSON, Account, Feed, Detection, Pagination})

  alias OrcasiteWeb.Resolvers

  query do
    @desc "Get logged in user"
    field(:current_user, :user, resolve: &Resolvers.Accounts.current_user/2)

    @desc "Get a list of feeds"
    field(:feeds, non_null(list_of(non_null(:feed))), resolve: &Resolvers.Feed.index/2)

    @desc "Get a feed"
    field :feed, non_null(:feed) do
      arg(:slug, :string)

      resolve(&Resolvers.Feed.show/2)
    end

    @desc "List detections"
    field :detections_all, list_of(:detection) do
      resolve(&Resolvers.Detection.index/2)
    end

    @desc "List detections, paginated"
    field :detections, :detections do
      arg(:pagination, :pagination)
      resolve(&Resolvers.Detection.list_detections/2)
    end

    @desc "List candidates, paginated"
    field :candidates, :candidates do
      arg(:pagination, :pagination)
      resolve(&Resolvers.Detection.list_candidates/2)
    end

    @desc "List users, paginated"
    field :users, :users do
      arg(:pagination, :pagination)
      resolve(&Resolvers.Accounts.list_users/2)
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

    @desc "Update user details"
    field :update_user, :user do
      arg(:id)
      arg(:admin, :boolean)
      resolve(&Resolvers.Accounts.update_user/2)
    end

    @desc "Submit an orca sound detection"
    field :submit_detection, :detection_with_lockout do
      arg(:feed_id, :id)
      arg(:playlist_timestamp, :string)
      arg(:player_offset, :decimal)
      arg(:listener_count, :integer)
      arg(:description, :string)

      resolve(&Resolvers.Detection.create/2)
    end
  end
end
