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

    read :index do
      pagination do
        offset? true
        countable true
        default_limit 100
      end

      argument :feed_id, :string

      filter expr(if not is_nil(^arg(:feed_id), do: feed_id == ^arg(:feed_id)), else: true)
    end

    create :from_m3u8_path do
      upsert? true
      upsert_identity :feed_stream_timestamp

      upsert_fields [
        :start_time,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :playlist_path,
        :playlist_m3u8_path,
        :playlist_timestamp,
        :updated_at
      ]

      argument :m3u8_path, :string, allow_nil?: false
      argument :feed, :map
      argument :playlist_path, :string

      change fn changeset, _context ->
        path =
          changeset
          |> Ash.Changeset.get_argument(:m3u8_path)
          |> String.trim_leading("/")

        with {:path, %{"node_name" => node_name, "timestamp" => playlist_timestamp}} <-
               {:path,
                Regex.named_captures(
                  ~r|(?<node_name>[^/]+)/hls/(?<timestamp>[^/]+)/live.m3u8|,
                  path
                )},
             {:feed, _, {:ok, feed}} <-
               {:feed, node_name, Orcasite.Radio.Feed.get_feed_by_node_name(node_name)} do
          changeset
          |> Ash.Changeset.manage_relationship(:feed, feed, type: :append)
          |> Ash.Changeset.change_attribute(:playlist_timestamp, playlist_timestamp)
          |> Ash.Changeset.set_argument(:feed, feed)
        else
          {:feed, node_name, _error} ->
            changeset
            |> Ash.Changeset.add_error("Feed for #{node_name} not found")

          {:path, _error} ->
            changeset
            |> Ash.Changeset.add_error(
              "Path #{path} does not match the hls object path format (<node_name>/hls/<timestamp>/live.m3u8)"
            )
        end
      end

      change fn changeset, context ->
        if !changeset.valid? do
          changeset
        else
          feed = Ash.Changeset.get_argument_or_attribute(changeset, :feed)

          playlist_timestamp =
            changeset
            |> Ash.Changeset.get_argument_or_attribute(:playlist_timestamp)

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
          |> Ash.Changeset.change_attribute(:playlist_timestamp, playlist_timestamp)
        end
      end
    end

    create :create do
      primary? true
      upsert? true
      upsert_identity :feed_stream_timestamp
      upsert_fields [
        :start_time,
        :end_time,
        :duration,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :playlist_timestamp,
        :updated_at
      ]

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

      change set_attribute(:playlist_timestamp, arg(:playlist_timestamp))
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
  end

  code_interface do
    define_for Orcasite.Radio

    define :create_from_m3u8_path, action: :from_m3u8_path, args: [:m3u8_path]
  end

  graphql do
    type :feed_stream

    queries do
      list :feed_streams, :index
    end
  end
end
