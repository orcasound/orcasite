defmodule Orcasite.Radio.Feed do
  use Ash.Resource,
    extensions: [AshAdmin.Resource, AshUUID, AshGraphql.Resource, AshJsonApi.Resource],
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer]

  postgres do
    table "feeds"
    repo Orcasite.Repo

    custom_indexes do
      index [:name]
      index [:node_name]
    end

    migration_defaults id: "fragment(\"uuid_generate_v7()\")"
  end

  identities do
    identity :unique_slug, [:slug]
  end

  attributes do
    uuid_attribute(:id, prefix: "feed")

    attribute :name, :string, allow_nil?: false
    attribute :node_name, :string, allow_nil?: false
    attribute :slug, :string, allow_nil?: false
    attribute :location_point, :geometry, allow_nil?: false
    attribute :intro_html, :string, default: ""
    attribute :image_url, :string, default: ""
    attribute :visible, :boolean, default: true

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    has_many :feed_streams, Orcasite.Radio.FeedStream
  end


  calculations do
    calculate :lat_lng,
              Orcasite.Types.LatLng,
              {Orcasite.Radio.Calculations.LatLng,
               keys: [:location_point], select: [:location_point]},
              allow_nil?: false

    calculate :lat_lng_string,
              :string,
              {Orcasite.Radio.Calculations.LatLng,
               return_type: :string, keys: [:location_point], select: [:location_point]}

    calculate :thumb_url,
              :string,
              {Orcasite.Radio.Calculations.FeedImageUrl, object: "thumbnail.png"}

    calculate :map_url,
              :string,
              {Orcasite.Radio.Calculations.FeedImageUrl, object: "map.png"}
  end

  policies do
    bypass actor_attribute_equals(:admin, true) do
      authorize_if always()
    end

    bypass action_type(:read) do
      authorize_if always()
    end
  end

  actions do
    defaults [:destroy]

    read :read do
      primary? true
      prepare build(load: [:lat_lng, :lat_lng_string])
    end

    read :index do
      filter expr(visible)
      prepare build(load: [:lat_lng, :lat_lng_string])
    end

    read :get_by_slug do
      get_by :slug
    end

    create :create do
      primary? true
      reject [:location_point]

      argument :lat_lng_string, :string do
        description "A comma-separated string of longitude and latitude"
      end

      change &change_lat_lng/2
    end

    update :update do
      primary? true
      reject [:location_point]

      argument :lat_lng_string, :string do
        description "A comma-separated string of longitude and latitude"
      end

      change &change_lat_lng/2
    end
  end

  admin do
    table_columns [:id, :name, :slug, :node_name, :location_point, :visible]

    format_fields location_point: {Jason, :encode!, []}, lat_lng: {Jason, :encode!, []}

    form do
      field :intro_html, type: :long_text
    end
  end

  code_interface do
    define_for Orcasite.Radio

    define :get_feed_by_slug, action: :get_by_slug, args: [:slug], get?: true
  end

  json_api do
    type "feed"

    routes do
      base "/feeds"

      index :index
    end
  end

  graphql do
    type :feed

    queries do
      read_one :feed, :get_by_slug, allow_nil?: false
      list :feeds, :index
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
