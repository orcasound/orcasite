defmodule Orcasite.Radio.AudioImage do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshGraphql.Resource, AshUUID],
    data_layer: AshPostgres.DataLayer,
    notifiers: [Ash.Notifier.PubSub]

  resource do
    description "Spectrograms or any other type of image representing audio."
  end

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
    identity :id, [:id]
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

  code_interface do
    define :generate_spectrogram
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
        max_page_size 1000
      end

      filter expr(
               feed_id == ^arg(:feed_id) and
                 fragment(
                   "(?) <= (?) AND (?) >= (?)",
                   start_time,
                   ^arg(:end_time),
                   end_time,
                   ^arg(:start_time)
                 )
             )
    end

    read :for_feed_subscription do
      argument :feed_id, :string, allow_nil?: false
      argument :start_time, :utc_datetime_usec, allow_nil?: false
      argument :end_time, :utc_datetime_usec, allow_nil?: false

      filter expr(
               feed_id == ^arg(:feed_id) and
                 fragment(
                   "(?) <= (?) AND (?) >= (?)",
                   start_time,
                   ^arg(:end_time),
                   end_time,
                   ^arg(:start_time)
                 )
             )
    end

    # A spectrogram covering one span of a feed's audio. Workers.
    # GenerateFeedSpectrograms calls this in bulk, so it runs no queries of its
    # own: the caller passes the feed's node name, and links the image to its
    # segments and queues its render afterwards.
    #
    # It is an upsert, and asking again for a span that already has an image
    # returns that image untouched. In particular the image keeps the object
    # path it has, whatever format the archive was rendered in, because only
    # `updated_at` is written on a conflict.
    create :for_span do
      upsert? true
      upsert_identity :unique_audio_image
      upsert_fields [:updated_at]

      accept [:feed_id, :start_time, :end_time]
      argument :node_name, :string, allow_nil?: false

      change set_attribute(:image_type, :spectrogram)

      change fn change, _context ->
        # Choose the id here rather than in the database, because the object
        # path is named for it.
        id = Ash.UUID.generate()

        change
        |> Ash.Changeset.force_change_attributes(%{
          id: id,
          bucket: Application.fetch_env!(:orcasite, :audio_image_bucket),
          bucket_region: Application.fetch_env!(:orcasite, :audio_image_bucket_region),
          object_path:
            Orcasite.Radio.AwsClient.spectrogram_object_path(
              Ash.Changeset.get_argument(change, :node_name),
              Ash.Changeset.get_attribute(change, :start_time),
              Ash.Changeset.get_attribute(change, :end_time),
              id
            )
        })
      end
    end

    create :seed do
      upsert? true
      upsert_identity :id

      skip_unknown_inputs :*

      accept [
        :image_type,
        :status,
        :start_time,
        :end_time,
        :parameters,
        :image_size,
        :bucket,
        :bucket_region,
        :object_path,
        :last_error
      ]

      argument :id, :string
      change Orcasite.Radio.Seed.Changes.KeepProductionId

      upsert_fields [
        :image_type,
        :status,
        :start_time,
        :end_time,
        :parameters,
        :image_size,
        :bucket,
        :bucket_region,
        :object_path,
        :last_error
      ]

      argument :feed, :map

      change manage_relationship(:feed, type: :append)
    end

    update :generate_spectrogram do
      require_atomic? false
      change set_attribute(:status, :processing)

      change after_transaction(fn _change, {:ok, image}, _context ->
               # Only one feed segment at a time for now
               [feed_segment] = image |> Ash.load!(:feed_segments) |> Map.get(:feed_segments)

               %{
                 image_id: image.id,
                 audio_bucket: feed_segment.bucket,
                 audio_key: feed_segment.segment_path,
                 audio_region: feed_segment.bucket_region,
                 image_bucket: image.bucket,
                 image_key: image.object_path,
                 image_region: image.bucket_region
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
        read_action :for_feed_subscription
        actions [:for_span, :update]
      end
    end
  end
end
