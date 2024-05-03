defmodule Orcasite.Radio.BoutFeedStream do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshGraphql.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  postgres do
    table "bout_feed_streams"
    repo Orcasite.Repo

    custom_indexes do
      index [:bout_id]
      index [:feed_stream_id]
    end
  end

  attributes do
    uuid_primary_key :id

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :bout, Orcasite.Radio.Bout
    belongs_to :feed_stream, Orcasite.Radio.FeedStream
  end

  resource do
    description "Join table between Bout and FeedStream"
  end

  actions do
    defaults [:read, :create, :update, :destroy]
  end

  graphql do
    type :bout_feed_stream
  end
end
