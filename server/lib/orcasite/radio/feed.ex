defmodule Orcasite.Radio.Feed do
  require Ash.Resource.Change.Builtins

  use Ash.Resource,
    domain: Orcasite.Radio,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  postgres do
    table "feeds"
    repo Orcasite.Repo

    custom_indexes do
      index [:name]
      index [:node_name]
      index [:visible]
      index [:slug]
      index [:dataplicity_id]
      index [:orcahello_id]
    end

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"
  end

  identities do
    identity :unique_slug, [:slug]
  end

  attributes do
    uuid_attribute :id, public?: true

    attribute :name, :string, allow_nil?: false, public?: true
    attribute :node_name, :string, allow_nil?: false, public?: true
    attribute :slug, :string, allow_nil?: false, public?: true
    attribute :location_point, :geometry, allow_nil?: false, public?: true
    attribute :intro_html, :string, default: "", public?: true
    attribute :image_url, :string, default: "", public?: true
    attribute :visible, :boolean, default: true, public?: true
    attribute :bucket, :string, public?: true, allow_nil?: false
    attribute :bucket_region, :string, public?: true
    attribute :cloudfront_url, :string, public?: true
    attribute :dataplicity_id, :string, public?: true
    attribute :orcahello_id, :string, public?: true

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  calculations do
    calculate :lat_lng,
              Orcasite.Types.LatLng,
              {Orcasite.Radio.Calculations.LatLng,
               keys: [:location_point], select: [:location_point]},
              allow_nil?: false,
              public?: true

    calculate :lat_lng_string,
              :string,
              {Orcasite.Radio.Calculations.LatLng,
               return_type: :string, keys: [:location_point], select: [:location_point]}

    calculate :thumb_url,
              :string,
              {Orcasite.Radio.Calculations.FeedImageUrl, object: "thumbnail.png"},
              public?: true

    calculate :map_url,
              :string,
              {Orcasite.Radio.Calculations.FeedImageUrl, object: "map.png"},
              public?: true

    calculate :uuid, :string, Orcasite.Radio.Calculations.DecodeUUID
  end

  aggregates do
    exists :online, :feed_segments do
      public? true
      filter expr(inserted_at > ago(5, :minute))
    end
  end

  relationships do
    has_many :feed_streams, Orcasite.Radio.FeedStream do
      public? true
    end

    has_many :feed_segments, Orcasite.Radio.FeedSegment do
      public? true
    end

    has_many :audio_images, Orcasite.Radio.AudioImage do
      public? true
    end

    has_many :bouts, Orcasite.Radio.Bout do
      public? true
    end
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass action_type(:read) do
      authorize_if always()
    end

    policy action(:generate_spectrogram) do
      authorize_if actor_attribute_equals(:moderator, true)
    end
  end

  actions do
    defaults [:read, :destroy]

    read :index do
      prepare build(load: [:lat_lng, :lat_lng_string, :online])
    end

    read :public do
      filter expr(visible)
      prepare build(load: [:lat_lng, :lat_lng_string, :online])
    end

    read :get_by_slug do
      get_by :slug
    end

    read :get_by_node_name do
      get_by :node_name
    end

    action :get_detections_count, :integer do
      argument :feed_id, :string, allow_nil?: false
      argument :from_time, :utc_datetime_usec, allow_nil?: false
      argument :to_time, :utc_datetime_usec
      argument :category, Orcasite.Types.DetectionCategory

      run fn %{arguments: %{feed_id: feed_id, from_time: from_time} = args}, _ ->
        category = args |> Map.get(:category)
        to_time = args |> Map.get(:to_time)

        import Ecto.Query

        {:ok,
         from(d in Orcasite.Radio.Detection,
           select: count(),
           where: d.feed_id == ^feed_id and d.timestamp >= ^from_time
         )
         |> then(fn query ->
           if not is_nil(to_time) do
             query
             |> where(
               [d],
               d.timestamp <= ^to_time
             )
           else
             query
           end
         end)
         |> then(fn query ->
           if not is_nil(category) do
             query
             |> where([d], d.category == ^category)
           else
             query
           end
         end)
         |> Orcasite.Repo.one()}
      end
    end

    create :create do
      primary? true

      accept [
        :name,
        :node_name,
        :slug,
        :intro_html,
        :image_url,
        :visible,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :dataplicity_id,
        :orcahello_id
      ]

      argument :lat_lng_string, :string do
        description "A comma-separated string of longitude and latitude"
      end

      change &change_lat_lng/2
    end

    update :update do
      primary? true
      require_atomic? false

      accept [
        :name,
        :node_name,
        :slug,
        :intro_html,
        :image_url,
        :visible,
        :bucket,
        :bucket_region,
        :cloudfront_url,
        :dataplicity_id,
        :orcahello_id
      ]

      argument :lat_lng_string, :string do
        description "A comma-separated string of longitude and latitude"
      end

      change &change_lat_lng/2
    end

    update :generate_spectrogram do
      require_atomic? false
      argument :start_time, :utc_datetime_usec, allow_nil?: false
      argument :end_time, :utc_datetime_usec, allow_nil?: false

      validate {Orcasite.Validations.Compare, [lt: [:start_time, :end_time]]}

      change before_action(fn change, _context ->
               # Get feed_segments between the start time and end time, and, for now
               # create a single spectrogram per segment. Once we can create an
               # audio image for multiple segments (with concatenation in the lambda),
               # we can change this to a single spectrogram with all those audio segments

               feed_segments =
                 Orcasite.Radio.FeedSegment
                 |> Ash.Query.for_read(:for_feed_range, %{
                   feed_id: change.data.id,
                   start_time: change.arguments.start_time,
                   end_time: change.arguments.end_time
                 })
                 |> Ash.read!(authorize?: false)

               audio_image_inputs = feed_segments |> Enum.map(&%{feed_segment_id: &1.id})

               change
               |> Ash.Changeset.manage_relationship(:audio_images, audio_image_inputs,
                 on_lookup: :ignore,
                 on_no_match: {:create, :for_feed_segment},
                 on_match: :ignore
               )
             end)
    end
  end

  code_interface do
    define :get_feed_by_slug, action: :get_by_slug, args: [:slug], get?: true
    define :get_feed_by_node_name, action: :get_by_node_name, args: [:node_name], get?: true
  end

  admin do
    table_columns [:id, :name, :slug, :node_name, :location_point, :visible, :online]

    format_fields location_point: {Jason, :encode!, []}, lat_lng: {Jason, :encode!, []}

    form do
      field :intro_html, type: :long_text
    end
  end

  json_api do
    type "feed"

    includes [:feed_streams]

    routes do
      base "/feeds"

      index :index
    end
  end

  graphql do
    type :feed

    queries do
      read_one :feed, :get_by_slug, allow_nil?: false
      action :feed_detections_count, :get_detections_count
      list :feeds, :public
    end

    mutations do
      update :generate_feed_spectrograms, :generate_spectrogram
    end
  end

  defp change_lat_lng(changeset, _context) do
    with {:is_string, lat_lng} when is_binary(lat_lng) <-
           {:is_string, Ash.Changeset.get_argument(changeset, :lat_lng_string)},
         {:two_els, [lat, lng]} <-
           {:two_els, lat_lng |> String.split(",") |> Enum.map(&String.trim/1)},
         {:two_floats, [{latitude, _}, {longitude, _}]} <-
           {:two_floats, [lat, lng] |> Enum.map(&Float.parse/1)} do
      changeset
      |> Ash.Changeset.change_attribute(:location_point, %Geo.Point{
        coordinates: {longitude, latitude},
        srid: 4326
      })
    else
      {:is_string, _} ->
        changeset

      {:two_els, _} ->
        changeset
        |> Ash.Changeset.add_error(
          field: :lat_lng,
          message: "must be a comma-separated string"
        )

      {:two_floats, _} ->
        changeset
        |> Ash.Changeset.add_error(field: :lat_lng, message: "must be two floats")
    end
  end
end
