defmodule Orcasite.Radio.FeedSegment do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer

  postgres do
    table "feed_segments"
    repo Orcasite.Repo

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"

    custom_indexes do
      index [:start_time]
      index [:end_time]
      index [:feed_id]
      index [:feed_stream_id]
      index [:bucket]
      index [:inserted_at]
    end
  end

  identities do
    identity :feed_segment_timestamp, [:feed_id, :start_time]
    identity :feed_segment_path, [:segment_path]
  end

  attributes do
    uuid_attribute :id, prefix: "fdseg", public?: true

    attribute :start_time, :utc_datetime_usec, public?: true
    attribute :end_time, :utc_datetime_usec, public?: true
    attribute :duration, :decimal, public?: true

    attribute :bucket, :string, public?: true
    attribute :bucket_region, :string, public?: true
    attribute :cloudfront_url, :string, public?: true

    attribute :playlist_timestamp, :string do
      public? true
      description "UTC Unix epoch for playlist (m3u8 dir) start (e.g. 1541027406)"
    end

    attribute :playlist_path, :string do
      public? true
      description "S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406/)"
    end

    attribute :playlist_m3u8_path, :string do
      public? true

      description "S3 object path for playlist file (e.g. /rpi_orcasound_lab/hls/1541027406/live.m3u8)"
    end

    attribute :segment_path, :string do
      public? true
      description "S3 object path for ts file (e.g. /rpi_orcasound_lab/hls/1541027406/live005.ts)"
    end

    attribute :file_name, :string do
      public? true
      description "ts file name (e.g. live005.ts)"
      allow_nil? false
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :feed, Orcasite.Radio.Feed, public?: true
    belongs_to :feed_stream, Orcasite.Radio.FeedStream, public?: true

    has_many :audio_image_feed_segments, Orcasite.Radio.AudioImageFeedSegment do
      public? true
    end

    many_to_many :audio_images, Orcasite.Radio.AudioImage do
      through Orcasite.Radio.AudioImageFeedSegment
      public? true
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
      argument :feed_stream_id, :string

      filter expr(
               if(not is_nil(^arg(:feed_id)), do: feed_id == ^arg(:feed_id), else: true) and
                 if(
                   not is_nil(^arg(:feed_stream_id)),
                   do: feed_stream_id == ^arg(:feed_stream_id),
                   else: true
                 )
             )
    end

    read :for_feed_range do
      argument :start_time, :utc_datetime_usec, allow_nil?: false
      argument :end_time, :utc_datetime_usec, allow_nil?: false
      argument :feed_id, :string, allow_nil?: false

      filter expr(
               feed_id == ^arg(:feed_id) and
                 (fragment(
                    "(?) between (?) and (?)",
                    start_time,
                    ^arg(:start_time),
                    ^arg(:end_time)
                  ) or
                    fragment(
                      "(?) between (?) and (?)",
                      end_time,
                      ^arg(:start_time),
                      ^arg(:end_time)
                    ))
             )
    end

    create :create do
      primary? true
      upsert? true
      upsert_identity :feed_segment_path

      upsert_fields [
        :playlist_path,
        :duration,
        :start_time,
        :end_time,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :playlist_timestamp,
        :playlist_m3u8_path,
        :file_name
      ]

      accept [
        :start_time,
        :end_time,
        :duration,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :playlist_timestamp,
        :playlist_m3u8_path,
        :playlist_path,
        :file_name
      ]

      argument :feed, :map, allow_nil?: false
      argument :feed_stream, :map, allow_nil?: false

      argument :segment_path, :string do
        allow_nil? false
        constraints allow_empty?: false
      end

      change set_attribute(:segment_path, arg(:segment_path))
      change manage_relationship(:feed, type: :append)
      change manage_relationship(:feed_stream, type: :append)

      change fn changeset, context ->
        feed = Ash.Changeset.get_argument_or_attribute(changeset, :feed)

        playlist_timestamp =
          changeset
          |> Ash.Changeset.get_attribute(:playlist_timestamp)

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
  end

  json_api do
    type "feed_segment"

    routes do
      base "/feed_segments"

      index :index
    end
  end

  graphql do
    type :feed_segment
    attribute_types [feed_id: :id, feed_stream_id: :id]

    queries do
      list :feed_segments, :index
    end
  end
end
