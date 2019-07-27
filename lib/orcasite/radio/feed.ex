defmodule Orcasite.Radio.Feed do
  use Ecto.Schema
  import Ecto.Changeset

  alias __MODULE__

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
    |> validate_required([:name, :node_name, :slug])
  end

  def latlong_to_geo(lat, long) when is_float(lat) and is_float(long),
    do: Geo.WKT.decode!("SRID=4326;POINT(#{lat} #{long})")

  # TODO: Find the actual json -> schema function
  def from_json(attrs) do
    %Feed{}
    |> cast(
      decode_location_point(attrs),
      Map.keys(Orcasite.Utils.atomize_keys(attrs))
    )
    |> apply_changes()
  end

  def decode_location_point(%{"location_point" => point} = attrs) when is_binary(point),
    do: %{attrs | "location_point" => Geo.WKB.decode!(attrs["location_point"])}

  def decode_location_point(attrs), do: attrs
end
