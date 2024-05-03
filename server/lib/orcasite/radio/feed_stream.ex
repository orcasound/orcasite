defmodule Orcasite.Radio.FeedStream do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer

  postgres do
    table "feed_streams"
    repo Orcasite.Repo

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"

    custom_indexes do
      index [:start_time]
      index [:end_time]
      index [:feed_id]
      index [:prev_feed_stream_id]
      index [:next_feed_stream_id]
    end
  end

  attributes do
    uuid_attribute :id, prefix: "fstrm"

    attribute :start_time, :utc_datetime
    attribute :end_time, :utc_datetime
    attribute :duration, :decimal
    attribute :bucket_url, :string

    attribute :playlist_path, :string do
      description "S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406)"
    end

    attribute :playlist_timestamp, :string do
      description "UTC Unix epoch for playlist start  (e.g. 1541027406)"
    end

    attribute :playlist_m3u8_path, :string do
      description "S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406/live.m3u8)"
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :feed, Orcasite.Radio.Feed
    belongs_to :prev_feed_stream, Orcasite.Radio.FeedStream
    belongs_to :next_feed_stream, Orcasite.Radio.FeedStream
    has_many :bout_feed_streams, Orcasite.Radio.BoutFeedStream

    many_to_many :bouts, Orcasite.Radio.Bout do
      through Orcasite.Radio.BoutFeedStream
    end
  end

  actions do
    defaults [:read, :update, :destroy]

    create :create do
      primary? true
      argument :feed, :string
      argument :prev_feed_stream, :string

      change manage_relationship(:feed, type: :append)
      change manage_relationship(:prev_feed_stream, type: :append)
    end

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      argument :feed_id, :string

      filter expr(if not is_nil(^arg(:feed_id), do: feed_id == ^arg(:feed_id)), else: true)
    end
  end

  graphql do
    type :feed_stream

    queries do
      list :feed_streams, :index
    end
  end
end
