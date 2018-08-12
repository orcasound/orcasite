defmodule Orcasite.Radio.Feed do
  use Ecto.Schema
  import Ecto.Changeset

  schema "feeds" do
    field(:name, :string)
    field(:slug, :string)
    field(:node_name, :string)
    field(:location_point, Geo.PostGIS.Geometry)

    timestamps()
  end

  @doc false
  def changeset(feed, attrs) do
    feed
    |> cast(attrs, [:name, :node_name, :slug, :location_point])
    |> validate_required([:name, :node_name, :slug, :location_point])
  end

  def latlong_to_geo(lat, long) when is_float(lat) and is_float(long),
    do: Geo.WKT.decode!("SRID=4326;POINT(#{lat} #{long})")
end
