defmodule Orcasite.Radio.AudioImageFeedSegment do
  use Ash.Resource,
    otp_app: :orcasite,
    domain: Orcasite.Radio,
    extensions: [AshGraphql.Resource, AshUUID],
    data_layer: AshPostgres.DataLayer

  postgres do
    table "audio_image_feed_segments"
    repo Orcasite.Repo

    custom_indexes do
      index [:feed_segment_id]
      index [:audio_image_id]
    end
  end

  identities do
    identity :unique_audio_image, [:feed_segment_id, :audio_image_id]
  end

  attributes do
    uuid_primary_key :id

    timestamps()
  end

  relationships do
    belongs_to :audio_image, Orcasite.Radio.AudioImage, public?: true
    belongs_to :feed_segment, Orcasite.Radio.FeedSegment, public?: true
  end

  actions do
    defaults [:read, :destroy, update: :*]

    create :create do
      primary? true
      accept :*
      upsert? true
      upsert_identity :unique_audio_image
    end
  end

  graphql do
    type :audio_image_feed_segment
    attribute_types feed_segment_id: :id
  end
end
