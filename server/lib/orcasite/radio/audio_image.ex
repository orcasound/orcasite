defmodule Orcasite.Radio.AudioImage do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshGraphql.Resource],
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

  attributes do
    uuid_primary_key :id
    attribute :image_type, Orcasite.Types.ImageType, public?: true
    attribute :status, Orcasite.Types.AudioImageStatus, default: :new, public?: true

    attribute :start_time, :utc_datetime_usec, public?: true
    attribute :end_time, :utc_datetime_usec, public?: true

    attribute :parameters, :map do
      public? true
      description "Parameters used for generating the image (e.g. n_fft for spectrograms, etc)"
      default %{}
    end

    attribute :image_size, :integer, public?: true

    attribute :bucket, :string, public?: true
    attribute :bucket_region, :string, public?: true
    attribute :object_path, :string

    timestamps()
  end

  relationships do
    belongs_to :feed, Orcasite.Radio.Feed do
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
  end

  graphql do
    type :audio_image
  end
end
