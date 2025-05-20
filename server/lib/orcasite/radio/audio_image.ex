defmodule Orcasite.Radio.AudioImage do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshGraphql.Resource, AshUUID],
    data_layer: AshPostgres.DataLayer,
    notifiers: [Ash.Notifier.PubSub]

  postgres do
    table "audio_images"
    repo Orcasite.Repo

    custom_indexes do
      index [:start_time]
      index [:end_time]
      index [:feed_id]
      index [:bucket]
      index [:image_type]
      index [:status]
    end
  end

  identities do
    identity :unique_audio_image, [:feed_id, :image_type, :start_time, :end_time]
  end

  attributes do
    uuid_primary_key :id
    attribute :image_type, Orcasite.Types.ImageType, public?: true

    attribute :status, Orcasite.Types.AudioImageStatus do
      default :new
      public? true
      allow_nil? false
    end

    attribute :start_time, :utc_datetime_usec do
      public? true
      allow_nil? false
    end

    attribute :end_time, :utc_datetime_usec do
      public? true
      allow_nil? false
    end

    attribute :parameters, :map do
      public? true
      description "Parameters used for generating the image (e.g. n_fft for spectrograms, etc)"
      default %{}
    end

    attribute :image_size, :integer, public?: true

    attribute :bucket, :string, public?: true
    attribute :bucket_region, :string, public?: true
    attribute :object_path, :string, public?: true
    attribute :last_error, :string

    timestamps()
  end

  relationships do
    belongs_to :feed, Orcasite.Radio.Feed do
      allow_nil? false
      public? true
    end

    has_many :audio_image_feed_segments, Orcasite.Radio.AudioImageFeedSegment do
      public? true
    end

    many_to_many :feed_segments, Orcasite.Radio.FeedSegment do
      through Orcasite.Radio.AudioImageFeedSegment
      public? true
    end
  end

  actions do
    defaults [:read, :destroy, create: :*, update: :*]

    read :for_feed do
      argument :feed_id, :string, allow_nil?: false
      argument :start_time, :utc_datetime_usec, allow_nil?: false
      argument :end_time, :utc_datetime_usec, allow_nil?: false

      pagination do
        offset? true
        countable true
        default_limit 100
      end

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

    create :for_feed_segment do
      upsert? true
      upsert_identity :unique_audio_image

      argument :feed_segment_id, :string, allow_nil?: false

      argument :image_type, Orcasite.Types.ImageType do
        default :spectrogram
      end

      change set_attribute(:image_type, arg(:image_type))

      change fn change, _context ->
        change
        |> Ash.Changeset.change_attribute(
          :bucket,
          Application.fetch_env!(:orcasite, :audio_image_bucket)
        )
        |> Ash.Changeset.change_attribute(
          :bucket_region,
          Application.fetch_env!(:orcasite, :audio_image_bucket_region)
        )
      end

      change before_action(fn change, _context ->
               feed_segment_id = Ash.Changeset.get_argument(change, :feed_segment_id)

               Orcasite.Radio.FeedSegment
               |> Ash.get(feed_segment_id, authorize?: false)
               |> case do
                 {:ok, feed_segment} ->
                   change
                   |> Ash.Changeset.force_change_attributes(%{
                     start_time: feed_segment.start_time,
                     end_time: feed_segment.end_time
                   })
                   |> Ash.Changeset.manage_relationship(:feed_segments, [feed_segment],
                     type: :append
                   )
                   |> Ash.Changeset.manage_relationship(:feed, feed_segment.feed_id,
                     type: :append
                   )

                 error ->
                   error
               end
             end)

      change after_action(
               fn _change, record, _context ->
                 feed = record |> Ash.load!(:feed) |> Map.get(:feed)

                 date_string = record.start_time |> DateTime.to_date() |> to_string()

                 # Convert to string like: "2024-09-10_17-41-53Z"
                 start_time_string =
                   record.start_time
                   |> DateTime.truncate(:second)
                   |> to_string()
                   |> String.replace(":", "-")
                   |> String.replace(" ", "_")

                 end_time_string =
                   record.end_time
                   |> DateTime.truncate(:second)
                   |> to_string()
                   |> String.replace(":", "-")
                   |> String.replace(" ", "_")

                 record
                 |> Ash.Changeset.for_update(:update, %{
                   object_path:
                     "/#{feed.node_name}/spectrograms/#{date_string}/#{start_time_string}__#{end_time_string}__#{record.id}.png"
                 })
                 |> Ash.update(authorize?: false)
               end,
               prepend?: true
             )

      change after_action(fn
               _change, %{status: :complete} = record, _context ->
                 {:ok, record}

               _change, record, _context ->
                 %{audio_image_id: record.id}
                 |> Orcasite.Radio.Workers.GenerateSpectrogram.new()
                 |> Oban.insert()

                 {:ok, record}
             end)
    end

    update :generate_spectrogram do
      require_atomic? false
      change set_attribute(:status, :processing)

      change after_action(fn _change, image, _context ->
               # Only one feed segment at a time for now
               [feed_segment] = image |> Ash.load!(:feed_segments) |> Map.get(:feed_segments)

               %{
                 image_id: image.id,
                 audio_bucket: feed_segment.bucket,
                 audio_key: feed_segment.segment_path,
                 image_bucket: image.bucket,
                 image_key: image.object_path
               }
               |> Orcasite.Radio.AwsClient.generate_spectrogram()
               |> case do
                 {:ok, %{file_size: image_size} = resp} ->
                   image
                   |> Ash.Changeset.for_update(:update, %{
                     status: :complete,
                     image_size: image_size,
                     parameters: Map.get(resp, :parameters, %{})
                   })
                   |> Ash.Changeset.force_change_attribute(:last_error, nil)
                   |> Ash.update(authorize?: false)

                 {:error, error} ->
                   image
                   |> Ash.Changeset.for_update(:update, %{
                     status: :errored
                   })
                   |> Ash.Changeset.force_change_attribute(:last_error, inspect(error))
                   |> Ash.update(authorize?: false)
               end
             end)
    end

    update :set_failed do
      change set_attribute(:status, :failed)
    end
  end

  graphql do
    type :audio_image
    attribute_types feed_id: :id

    queries do
      list :audio_images, :for_feed
    end

    subscriptions do
      pubsub OrcasiteWeb.Endpoint

      subscribe :audio_image_updated do
        read_action :for_feed
        actions [:for_feed_segment, :update]
      end
    end
  end
end
