defmodule Orcasite.Radio.Bout do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  postgres do
    table "bouts"
    repo Orcasite.Repo

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"

    custom_indexes do
      index [:feed_id]
      index [:created_by_user_id]
    end
  end

  attributes do
    uuid_attribute :id, prefix: "bout", public?: true

    attribute :start_time, :utc_datetime_usec, public?: true
    attribute :end_time, :utc_datetime_usec, public?: true
    attribute :duration, :decimal, public?: true
    attribute :ongoing, :boolean, public?: true

    attribute :category, Orcasite.Types.AudioCategory do
      public? true
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :created_by_user, Orcasite.Accounts.User

    belongs_to :feed, Orcasite.Radio.Feed
    has_many :bout_feed_streams, Orcasite.Radio.BoutFeedStream

    many_to_many :feed_streams, Orcasite.Radio.FeedStream do
      through Orcasite.Radio.BoutFeedStream
    end
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass action_type(:read) do
      authorize_if always()
    end

    bypass action_type(:create) do
      authorize_if actor_attribute_equals(:moderator, true)
    end

    bypass action_type(:update) do
      authorize_if actor_attribute_equals(:moderator, true)
    end
  end

  actions do
    defaults [:read, :update, :destroy]

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      argument :feed_id, :string

      filter expr(if not is_nil(^arg(:feed_id), do: feed_id == ^arg(:feed_id)), else: true)
    end

    create :create do
      primary? true

      change fn
        changeset, %{actor: %Orcasite.Accounts.User{} = actor} ->
          changeset
          |> Ash.Changeset.manage_relationship(:created_by_user, actor, type: :append)

        changeset, _ ->
          changeset
      end
    end
  end

  graphql do
    type :bout

    queries do
      list :bouts, :index
    end

    mutations do
      create :create_bout, :create
    end
  end
end
