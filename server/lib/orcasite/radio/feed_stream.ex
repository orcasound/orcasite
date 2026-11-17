defmodule Orcasite.Radio.FeedStream do
  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer

  require Logger

  resource do
    description """
      Represents an m3u8 file in S3 (audio segment manifest, lists .ts files in order with their length).
      Whenever a feed restarts, a new m3u8 file is created
    """
  end

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
    uuid_attribute :id,
      prefix: "fdstrm",
      public?: true

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
      writable? true
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

  code_interface do
    define :create_from_m3u8_path, action: :from_m3u8_path, args: [:m3u8_path]
    define :for_timestamp, action: :for_timestamp
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

    read :for_timestamp do
      get? true

      argument :timestamp, :utc_datetime_usec, allow_nil?: false
      argument :feed_id, :string, allow_nil?: false

      prepare build(sort: [start_time: :asc], limit: 1)

      filter expr(
               feed_id == ^arg(:feed_id) and
                 start_time <= ^arg(:timestamp) and
                 start_time >= datetime_add(^arg(:timestamp), -1, :day) and
                 (is_nil(end_time) or end_time >= ^arg(:timestamp))
             )
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

      change manage_relationship(:feed, type: :append)

      change fn changeset, _context ->
        path =
          changeset
          |> Ash.Changeset.get_argument(:m3u8_path)
          |> String.trim_leading("/")

        with {:path, %{"node_name" => node_name, "timestamp" => playlist_timestamp}} <-
               {:path,
                Regex.named_captures(
                  ~r{(?<node_name>[^/]+)/hls/(?<timestamp>[^/]+)/(?:live|archive)\.m3u8$},
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
              "Path #{path} does not match the hls object path format (<node_name>/hls/<timestamp>/live.m3u8 or archive.m3u8)"
            )
        end
      end

      change fn changeset, context ->
        if !changeset.valid? do
          changeset
        else
          feed_id =
            Ash.Changeset.get_argument_or_attribute(changeset, :feed)
            |> then(&(&1 |> Map.get(:id) || &1 |> Map.get("id")))

          feed = Orcasite.Radio.Feed |> Ash.get!(feed_id)

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
      upsert_identity :playlist_m3u8_path
      skip_unknown_inputs :*

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
        feed_id =
          Ash.Changeset.get_argument_or_attribute(changeset, :feed)
          |> then(&(&1 |> Map.get(:id) || &1 |> Map.get("id")))

        feed = Orcasite.Radio.Feed |> Ash.get!(feed_id)

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
      skip_unknown_inputs :*

      accept [
        :start_time,
        :end_time,
        :duration,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :playlist_path,
        :playlist_timestamp,
        :playlist_m3u8_path,
        :feed_id
      ]

      argument :id, :string
      change Orcasite.Radio.Seed.Changes.KeepProductionId

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
        :updated_at,
        :feed_id
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

               # Tests pass their own fetch in the changeset's context.
               get_body =
                 change.context[:get_manifest_body] ||
                   (&Orcasite.Radio.AwsClient.get_stream_manifest_body/1)

               with {:ok, feed_segments} <-
                      request_and_parse_manifest(feed_stream, feed, get_body) do
                 existing_file_names =
                   existing_feed_segments |> MapSet.new(& &1.file_name)

                 # Only insert segments not already present
                 insert_segments =
                   feed_segments
                   |> Enum.reject(&MapSet.member?(existing_file_names, &1.file_name))

                 insert_segments
                 |> Ash.bulk_create(Orcasite.Radio.FeedSegment, :create,
                   upsert?: true,
                   upsert_identity: :feed_segment_path,
                   stop_on_error?: true
                 )
                 |> case do
                   %{status: :success} ->
                     correct_end_time_and_duration(feed_stream, feed_segments)

                   %{errors: error} ->
                     {:error, error}
                 end
               else
                 # A playlist that is refused stays refused until the node
                 # uploads a different one, so failing here would only make
                 # the job retry. Nothing is ingested, and it is logged.
                 {:error, {reason, _details} = refusal}
                 when reason in [:rolling_playlist, :skipped_segments] ->
                   log_refusal_once(feed_stream, refusal)
                   {:ok, feed_stream}

                 other ->
                   other
               end
             end)
    end

    update :set_end_time_and_duration do
      description "Stores an end time and duration computed from the stream's playlist"
      accept [:end_time, :duration]
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
                 {:ok, [_ | _] = feed_segments} ->
                   {end_time, duration} = end_time_and_duration(feed_stream, feed_segments)

                   change
                   |> Ash.Changeset.change_attribute(:end_time, end_time)
                   |> Ash.Changeset.change_attribute(:duration, duration)

                 {:ok, []} ->
                   change

                 {:error, reason} ->
                   Ash.Changeset.add_error(change, inspect(reason))
               end
             end)
    end
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

  @doc """
  Fetches the stream's HLS manifest from S3 and parses it into FeedSegment
  attribute maps, ordered by segment start time.

  Nodes running orcanode's `node_2026` code since October 2026 write a
  `live.m3u8` that lists only the most recent segments, for the live player,
  and an `archive.m3u8` beside it that lists every segment since the folder
  started (#1092). When `live.m3u8` turns out to list only recent segments,
  this reads `archive.m3u8` instead. The node uploads the archive about every
  five minutes, so segments from the last few minutes appear on a later run.
  For the first minutes of a folder there is no archive yet; that returns
  `{:ok, []}`, because there is nothing new to read, not an error.

  `get_body` fetches a stream's manifest body; tests replace it.
  """
  @spec request_and_parse_manifest(t(), map() | nil, (t() -> {:ok, String.t()} | {:error, term()})) ::
          {:ok, [map()]} | {:error, term()}
  def request_and_parse_manifest(
        feed_stream,
        feed \\ nil,
        get_body \\ &Orcasite.Radio.AwsClient.get_stream_manifest_body/1
      ) do
    with {:ok, body} <- get_body.(feed_stream) do
      case parse_manifest(body, feed_stream, feed) do
        {:error, {:rolling_playlist, _media_sequence}} ->
          archive_stream = %{feed_stream | playlist_m3u8_path: archive_m3u8_path(feed_stream)}

          case get_body.(archive_stream) do
            {:ok, archive_body} -> parse_manifest(archive_body, feed_stream, feed)
            {:error, {:http_error, 404, _response}} -> {:ok, []}
            {:error, error} -> {:error, error}
          end

        result ->
          result
      end
    end
  end

  defp archive_m3u8_path(%{playlist_path: playlist_path}), do: playlist_path <> "archive.m3u8"

  @doc """
  Parses an HLS media playlist body into FeedSegment attribute maps.

  Segment times are derived by accumulating `#EXTINF` durations from the
  stream's `start_time`, which comes from the S3 folder name. When a segment
  is preceded by an `#EXT-X-PROGRAM-DATE-TIME` tag, its timestamp is carried
  as `program_date_time` but does not affect the computed times (see #1041).

  Those times are right only if the playlist starts with the folder's first
  segment. A playlist whose `#EXT-X-MEDIA-SEQUENCE` is above 0 has dropped
  its earliest segments, so it returns `{:error, {:rolling_playlist, n}}`
  rather than placing its segments at the start of the folder.

  They are also right only if no segment is missing from the middle. Some
  `archive.m3u8` files skip segments (`live2734.ts` followed by
  `live2751.ts`), which would place every later segment too early. A playlist
  whose `liveN.ts` numbers are not consecutive, or do not start at 0, returns
  `{:error, {:skipped_segments, details}}`.
  """
  @spec parse_manifest(String.t(), t(), map() | nil) :: {:ok, [map()]} | {:error, term()}
  def parse_manifest(body, feed_stream, feed \\ nil) do
    with {:ok, %ExM3U8.MediaPlaylist{timeline: timeline, info: info}} <-
           ExM3U8.deserialize_media_playlist(body, []),
         :ok <- check_starts_at_first_segment(info) do
      {segments, _offset_ms, _program_date_time} =
        Enum.reduce(timeline, {[], Decimal.new(0), nil}, fn
          %ExM3U8.Tags.ProgramDateTime{date: date}, {acc, offset_ms, _} ->
            {acc, offset_ms, date}

          %ExM3U8.Tags.Segment{uri: file_name, duration: duration_float},
          {acc, start_offset_ms, program_date_time} ->
            duration = Decimal.from_float(duration_float)
            end_offset_ms = duration |> Decimal.mult(1000) |> Decimal.add(start_offset_ms)

            segment =
              %{
                start_time: offset_time(feed_stream.start_time, start_offset_ms),
                end_time: offset_time(feed_stream.start_time, end_offset_ms),
                duration: duration,
                program_date_time: program_date_time,
                bucket: feed_stream.bucket,
                bucket_region: feed_stream.bucket_region,
                cloudfront_url: feed_stream.cloudfront_url,
                playlist_timestamp: feed_stream.playlist_timestamp,
                playlist_m3u8_path: feed_stream.playlist_m3u8_path,
                playlist_path: feed_stream.playlist_path,
                file_name: file_name,
                segment_path: feed_stream.playlist_path <> file_name,
                feed: feed,
                feed_stream: feed_stream
              }

            {[segment | acc], end_offset_ms, nil}

          _other_tag, state ->
            state
        end)

      segments = Enum.reverse(segments)

      with :ok <- check_no_skipped_segments(segments) do
        {:ok, segments}
      end
    end
  end

  # File names that don't follow the nodes' `live<number>.ts` pattern are not checked.
  defp check_no_skipped_segments(segments) do
    numbered =
      Enum.map(segments, fn %{file_name: file_name} ->
        case Regex.run(~r/^live(\d+)\.ts$/, file_name) do
          [_, number] -> {String.to_integer(number), file_name}
          nil -> nil
        end
      end)

    skips =
      if Enum.any?(numbered, &is_nil/1) do
        []
      else
        numbered
        |> Enum.zip(Enum.drop(numbered, 1))
        |> Enum.reject(fn {{number, _}, {next, _}} -> next == number + 1 end)
      end

    case {numbered, skips} do
      {[{first, file_name} | _], _} when first > 0 ->
        {:error, {:skipped_segments, %{count: 1, first: {nil, file_name}}}}

      {_, []} ->
        :ok

      {_, [{{_, file_name}, {_, next_file_name}} | _]} ->
        {:error, {:skipped_segments, %{count: length(skips), first: {file_name, next_file_name}}}}
    end
  end

  # A playlist is read every time the node uploads one, about every ten
  # seconds, so a refusal is logged once a day for each stream and reason.
  defp log_refusal_once(feed_stream, {reason, _details} = refusal) do
    if Orcasite.Cache.put_new({:refused_playlist, feed_stream.id, reason}, true,
         ttl: :timer.hours(24)
       ) do
      Logger.error(
        "Not ingesting segments for #{feed_stream.playlist_path}: " <>
          "its playlist was refused (#{inspect(refusal)}). See orcasite#1092."
      )
    end
  end

  defp end_time_and_duration(feed_stream, [_ | _] = feed_segments) do
    duration =
      feed_segments
      |> Enum.map(&Map.get(&1, :duration))
      |> Enum.reduce(&Decimal.add/2)

    {offset_time(feed_stream.start_time, Decimal.mult(duration, 1000)), duration}
  end

  # The end time and duration are first computed when the next stream is
  # linked, which can be before this stream's last playlist upload has been
  # read. Once a stream has ended, each later read of its playlist corrects
  # them, but only ever lengthens the stream.
  defp correct_end_time_and_duration(%{next_feed_stream_id: nil} = feed_stream, _segments),
    do: {:ok, feed_stream}

  defp correct_end_time_and_duration(feed_stream, []), do: {:ok, feed_stream}

  defp correct_end_time_and_duration(feed_stream, feed_segments) do
    {end_time, duration} = end_time_and_duration(feed_stream, feed_segments)

    if is_nil(feed_stream.duration) or Decimal.gt?(duration, feed_stream.duration) do
      feed_stream
      |> Ash.Changeset.for_update(:set_end_time_and_duration, %{
        end_time: end_time,
        duration: duration
      })
      |> Ash.update(authorize?: false)
    else
      {:ok, feed_stream}
    end
  end

  defp check_starts_at_first_segment(%{media_sequence: media_sequence})
       when is_integer(media_sequence) and media_sequence > 0,
       do: {:error, {:rolling_playlist, media_sequence}}

  defp check_starts_at_first_segment(_info), do: :ok

  defp offset_time(start_time, offset_ms) do
    DateTime.add(start_time, offset_ms |> Decimal.round() |> Decimal.to_integer(), :millisecond)
  end
end
