defmodule Orcasite.Radio.FeedStream do
  use Ash.Resource,
    domain: Orcasite.Radio,
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
    identity :playlist_m3u8_path, [:playlist_m3u8_path]
  end

  attributes do
    uuid_attribute :id, prefix: "fdstrm", public?: true

    attribute :start_time, :utc_datetime_usec, public?: true
    attribute :end_time, :utc_datetime_usec, public?: true
    attribute :duration, :decimal, public?: true

    attribute :bucket, :string, public?: true
    attribute :bucket_region, :string, public?: true
    attribute :cloudfront_url, :string, public?: true

    attribute :playlist_timestamp, :string do
      public? true
      description "UTC Unix epoch for playlist start (e.g. 1541027406)"
    end

    attribute :playlist_path, :string do
      public? true
      description "S3 object path for playlist dir (e.g. /rpi_orcasound_lab/hls/1541027406/)"
    end

    attribute :playlist_m3u8_path, :string do
      public? true

      description "S3 object path for playlist file (e.g. /rpi_orcasound_lab/hls/1541027406/live.m3u8)"
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :feed, Orcasite.Radio.Feed do
      public? true
    end

    belongs_to :prev_feed_stream, Orcasite.Radio.FeedStream do
      public? true
    end

    belongs_to :next_feed_stream, Orcasite.Radio.FeedStream do
      public? true
    end

    has_many :feed_segments, Orcasite.Radio.FeedSegment do
      public? true
    end

    has_many :bout_feed_streams, Orcasite.Radio.BoutFeedStream do
      public? true
    end

    many_to_many :bouts, Orcasite.Radio.Bout do
      public? true
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

      filter expr(if not is_nil(^arg(:feed_id)), do: feed_id == ^arg(:feed_id), else: true)
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
      argument :update_segments?, :boolean, default: false
      argument :link_streams?, :boolean, default: false
      argument :bucket, :string

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

          bucket_arg = Ash.Changeset.get_argument(changeset, :bucket)

          feed
          |> Map.take([:bucket_region, :cloudfront_url])
          |> Enum.reduce(changeset, fn {attribute, value}, acc ->
            acc
            |> Ash.Changeset.change_new_attribute(attribute, value)
          end)
          |> Ash.Changeset.change_new_attribute(:bucket, bucket_arg || feed.bucket)
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
          |> Ash.Changeset.after_action(fn change, %{id: feed_stream_id} = feed_stream ->
            if Ash.Changeset.get_argument(changeset, :update_segments?) do
              %{feed_stream_id: feed_stream_id}
              |> Orcasite.Radio.Workers.UpdateFeedSegments.new()
              |> Oban.insert()
            end

            if Ash.Changeset.get_argument(changeset, :link_streams?) do
              %{
                feed_stream_id: feed_stream_id,
                enqueue_next_stream: true,
                enqueue_prev_stream: true,
                prev_depth: 3,
                next_depth: 3
              }
              |> Orcasite.Radio.Workers.LinkFeedStream.new()
              |> Oban.insert()
            end

            {:ok, feed_stream}
          end)
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

      argument :update_segments?, :boolean, default: false
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
        |> Ash.Changeset.after_action(fn change, %{id: feed_stream_id} = feed_stream ->
          if Ash.Changeset.get_argument(changeset, :update_segments?) do
            %{feed_stream_id: feed_stream_id}
            |> Orcasite.Radio.Workers.UpdateFeedSegments.new()
            |> Oban.insert()
          end

          {:ok, feed_stream}
        end)
      end
    end

    create :populate_with_segments do
      upsert? true
      upsert_identity :playlist_m3u8_path

      accept [
        :start_time,
        :end_time,
        :duration,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :playlist_path,
        :playlist_timestamp,
        :playlist_m3u8_path
      ]

      upsert_fields [
        :start_time,
        :end_time,
        :duration,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :playlist_path,
        :playlist_timestamp,
        :playlist_m3u8_path,
        :updated_at
      ]

      argument :feed_segments, {:array, :map}
      argument :feed, :map

      change manage_relationship(:feed_segments, type: :create)
      change manage_relationship(:feed, type: :append)
    end

    update :update_segments do
      description "Pulls contents of m3u8 file and creates a FeedSegment per new entry"
      require_atomic? false
      validate present([:playlist_m3u8_path, :playlist_path])

      change after_action(fn change, feed_stream, _context ->
               file_name_query =
                 Orcasite.Radio.FeedSegment |> Ash.Query.new() |> Ash.Query.select([:file_name])

               %{feed: feed, feed_segments: existing_feed_segments} =
                 feed_stream |> Ash.load!([:feed, feed_segments: file_name_query])

               feed_segments = request_and_parse_manifest(feed_stream, feed)

               existing_file_names = existing_feed_segments |> Enum.map(& &1.file_name)

               # Only insert segments not already present
               insert_segments =
                 feed_segments
                 |> Enum.filter(&(&1.file_name not in existing_file_names))

               insert_segments
               |> Ash.bulk_create(Orcasite.Radio.FeedSegment, :create,
                 upsert?: true,
                 upsert_identity: :feed_segment_path,
                 stop_on_error?: true
               )
               |> case do
                 %{status: :success} -> {:ok, feed_stream}
                 %{errors: error} -> {:error, error}
               end
             end)
    end

    update :link_next_stream do
      require_atomic? false
      validate present([:start_time])
      validate absent([:next_feed_stream_id])

      change fn %{data: %{start_time: start_time, feed_id: feed_id}} = change, _context ->
        require Ash.Query

        Orcasite.Radio.FeedStream
        |> Ash.Query.filter(start_time > ^start_time and feed_id == ^feed_id)
        |> Ash.Query.sort(start_time: :asc)
        |> Ash.Query.limit(1)
        |> Ash.read()
        |> case do
          {:ok, [next_stream]} ->
            change
            |> Ash.Changeset.manage_relationship(:next_feed_stream, next_stream,
              on_lookup: :relate
            )

          _ ->
            change
        end
      end
    end

    update :link_prev_stream do
      require_atomic? false
      validate present([:start_time])
      validate absent([:prev_feed_stream_id])

      change fn %{data: %{start_time: start_time, feed_id: feed_id}} = change, _context ->
        require Ash.Query

        Orcasite.Radio.FeedStream
        |> Ash.Query.filter(start_time < ^start_time and feed_id == ^feed_id)
        |> Ash.Query.sort(start_time: :desc)
        |> Ash.Query.limit(1)
        |> Ash.read()
        |> case do
          {:ok, [prev_stream]} ->
            change
            |> Ash.Changeset.manage_relationship(:prev_feed_stream, prev_stream,
              on_lookup: :relate
            )

          _ ->
            change
        end
      end
    end

    update :update_end_time_and_duration do
      description "Pulls and parses the manifest body, updates duration and end time. Only runs if there's a next_feed_stream"
      require_atomic? false
      validate present([:next_feed_stream_id, :start_time])
      validate absent([:duration, :end_time])

      change before_action(fn %{data: feed_stream} = change, _context ->
               feed_stream
               |> request_and_parse_manifest()
               |> case do
                 feed_segments when is_list(feed_segments) and length(feed_segments) > 0 ->
                   duration =
                     feed_segments
                     |> Enum.map(&Map.get(&1, :duration))
                     |> Enum.reduce(&Decimal.add/2)

                   end_time =
                     DateTime.add(
                       feed_stream.start_time,
                       duration
                       |> Decimal.mult(1000)
                       |> Decimal.round()
                       |> Decimal.to_integer(),
                       :millisecond
                     )

                   change
                   |> Ash.Changeset.change_attribute(:end_time, end_time)
                   |> Ash.Changeset.change_attribute(:duration, duration)

                 _ ->
                   change
               end
             end)
    end
  end

  code_interface do
    define :create_from_m3u8_path, action: :from_m3u8_path, args: [:m3u8_path]
  end

  json_api do
    type "feed_stream"

    routes do
      base "/feed_streams"

      index :index
    end
  end

  graphql do
    type :feed_stream
    attribute_types feed_id: :string, prev_feed_stream_id: :string, next_feed_stream_id: :string

    queries do
      list :feed_streams, :index
    end
  end

  def request_and_parse_manifest(feed_stream, feed \\ nil) do
    playlist_start_time = feed_stream.start_time
    playlist_path = feed_stream.playlist_path

    {:ok, body} = Orcasite.Radio.AwsClient.get_stream_manifest_body(feed_stream)

    body
    |> String.split("#")
    # Looks like "EXTINF:10.005378,\nlive000.ts\n"
    |> Enum.filter(&String.contains?(&1, "EXTINF"))
    |> Enum.reduce(
      [],
      fn extinf_string, acc ->
        with %{"duration" => duration_string, "file_name" => file_name} <-
               Regex.named_captures(
                 ~r|EXTINF:(?<duration>[^,]+),\n(?<file_name>[^\n]+)|,
                 extinf_string
               ) do
          duration = Decimal.new(duration_string)

          start_offset =
            Enum.map(acc, & &1.duration)
            |> Enum.reduce(Decimal.new("0"), &Decimal.add/2)
            |> Decimal.mult(1000)

          end_offset =
            duration
            |> Decimal.mult(1000)
            |> Decimal.add(start_offset)
            |> Decimal.round()

          start_time =
            DateTime.add(
              playlist_start_time,
              Decimal.to_integer(Decimal.round(start_offset)),
              :millisecond
            )

          end_time =
            DateTime.add(
              playlist_start_time,
              Decimal.to_integer(end_offset),
              :millisecond
            )

          [
            %{
              start_time: start_time,
              end_time: end_time,
              duration: duration,
              bucket: feed_stream.bucket,
              bucket_region: feed_stream.bucket_region,
              cloudfront_url: feed_stream.cloudfront_url,
              playlist_timestamp: feed_stream.playlist_timestamp,
              playlist_m3u8_path: feed_stream.playlist_m3u8_path,
              playlist_path: playlist_path,
              file_name: file_name,
              segment_path: playlist_path <> file_name,
              feed: feed,
              feed_stream: feed_stream
            }
            | acc
          ]
        else
          _ -> acc
        end
      end
    )
  end
end
