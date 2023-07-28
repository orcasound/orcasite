defmodule Orcasite.Radio.Feed do
  use Ash.Resource,
    extensions: [AshAdmin.Resource],
    data_layer: AshPostgres.DataLayer

  attributes do
    integer_primary_key :id

    attribute :name, :string
    attribute :node_name, :string
    attribute :slug, :string
    attribute :location_point, :geometry, allow_nil?: false

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  postgres do
    table "feeds"
    repo Orcasite.Repo
  end

  identities do
    identity :unique_slug, [:slug]
  end

  actions do
    defaults [:destroy]

    read :read do
      primary? true

      prepare fn query, _context ->
        query
        |> Ash.Query.load(:lat_lng)
      end
    end

    read :get_by_slug do
      get_by :slug
    end

    create :create do
      primary? true
      reject [:location_point]

      argument :lat_lng, :string do
        description "A comma-separated string of longitude and latitude"
      end

      change &change_lat_lng/2
    end

    update :update do
      primary? true
      reject [:location_point]

      argument :lat_lng, :string do
        description "A comma-separated string of longitude and latitude"
      end

      change &change_lat_lng/2
    end
  end

  code_interface do
    define_for Orcasite.Radio

    define :get_feed_by_slug, action: :get_by_slug, args: [:slug], get?: true
  end

  calculations do
    calculate :lat_lng,
              :string,
              {Orcasite.Radio.Calculations.LatLng,
               keys: [:location_point], select: [:location_point]}
  end

  defp change_lat_lng(changeset, _context) do
    with {:is_string, lat_lng} when is_binary(lat_lng) <-
           {:is_string, Ash.Changeset.get_argument(changeset, :lat_lng)},
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

  admin do
    table_columns [:id, :name, :slug, :node_name, :location_point]

    format_fields location_point: {Jason, :encode!, []}
  end
end
