defmodule Orcasite.Radio.AudioImage do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshGraphql.Resource, AshUUID],
    data_layer: AshPostgres.DataLayer

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

    create :for_feed_segment do
      argument :feed_segment_id, :string, allow_nil?: false

      argument :image_type, Orcasite.Types.ImageType do
        default :spectrogram
      end

      change set_attribute(:image_type, arg(:image_type))
      change set_attribute(:bucket, "dev-audio-viz")
      change set_attribute(:bucket_region, "us-west-2")

      change before_action(fn change, _context ->
               feed_segment_id = Ash.Changeset.get_argument(change, :feed_segment_id)

               Orcasite.Radio.FeedSegment
               |> Ash.get(feed_segment_id, authorize?: false)
               |> case do
                 {:ok, feed_segment} ->
                   change
                   |> Ash.Changeset.change_attributes(%{
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

                 record
                 |> Ash.Changeset.for_update(:update, %{
                   object_path: "/#{feed.node_name}/spectrograms/#{record.id}.png"
                 })
                 |> Ash.update(authorize?: false)
               end,
               prepend?: true
             )

      change after_action(fn _change, record, _context ->
               %{audio_image_id: record.id}
               |> Orcasite.Radio.Workers.GenerateSpectrogram.new()
               |> Oban.insert()

               {:ok, record}
             end)
    end

    update :generate_spectrogram do
      change set_attribute(:status, :processing)

      change after_action(
               fn _change, image, _context ->
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
                 |> IO.inspect(label: "gen spect result")
                 |> case do
                   {:ok, %{image_size: image_size, sample_rate: _sample_rate}} ->
                     image
                     |> Ash.Changeset.for_update(:update, %{
                       status: :complete,
                       image_size: image_size
                     })
                     |> Ash.Changeset.force_change_attribute(:last_error, nil)
                     |> Ash.update(authorize?: false)

                     {:ok, image}

                   {:error, error} ->
                     image
                     |> Ash.Changeset.for_update(:update, %{
                       status: :failed
                     })
                     |> Ash.Changeset.force_change_attribute(:last_error, inspect(error))
                     |> Ash.update(authorize?: false)

                    #  error
                    {:ok, image}
                 end
               end,
               prepend?: true
             )
    end
  end

  graphql do
    type :audio_image
  end
end
