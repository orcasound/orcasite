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
      index [:bucket]
    end
  end

  identities do
    identity :feed_stream_timestamp, [:feed_id, :start_time]
  end

  attributes do
    uuid_attribute :id, prefix: "fdstrm"

    attribute :start_time, :utc_datetime
    attribute :end_time, :utc_datetime
    attribute :duration, :decimal

    attribute :bucket, :string
    attribute :bucket_region, :string
    attribute :cloudfront_url, :string

    attribute :playlist_timestamp, :string do
      description "UTC Unix epoch for playlist start (e.g. 1541027406)"
    end

    attribute :playlist_path, :string do
      description "S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406/)"
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

    create :from_node_name do
      upsert? true
      upsert_identity :feed_stream_timestamp
      argument :node_name, :string, allow_nil?: false
      argument :playlist_timestamp, :string, allow_nil?: false
    end

    create :create do
      primary? true
      upsert? true
      upsert_identity :feed_stream_timestamp

      accept [
        :start_time,
        :end_time,
        :duration,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :playlist_timestamp
      ]

      argument :feed, :map, allow_nil?: false
      argument :prev_feed_stream, :string

      argument :playlist_timestamp, :string do
        allow_nil? false
        constraints allow_empty?: false
      end

      change manage_relationship(:feed, type: :append)
      change manage_relationship(:prev_feed_stream, type: :append)

      change fn changeset, context ->
        feed = Ash.Changeset.get_argument_or_attribute(changeset, :feed)

        playlist_timestamp =
          changeset
          |> Ash.Changeset.get_argument(:playlist_timestamp)

        feed
        |> Map.take([:bucket, :bucket_region, :cloudfront_url])
        |> Enum.reduce(changeset, fn {attribute, value}, acc ->
          acc
          |> Ash.Changeset.change_new_attribute(attribute, value)
        end)
        |> Ash.Changeset.change_new_attribute(
          :start_time,
          playlist_timestamp
          |> String.to_integer()
          |> DateTime.from_unix!()
        )
        |> Ash.Changeset.change_new_attribute(
          :playlist_path,
          "/#{feed.node_name}/hls/#{playlist_timestamp}/"
        )
        |> Ash.Changeset.change_new_attribute(
          :playlist_m3u8_path,
          "/#{feed.node_name}/hls/#{playlist_timestamp}/live.m3u8"
        )
      end
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
