defmodule Orcasite.Types.Geometry do
  @moduledoc false

  use Ash.Type

  @impl true
  def storage_type, do: :geometry

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, _) do
    Geo.PostGIS.Geometry.cast(value)
  end

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, _) do
    Geo.PostGIS.Geometry.load(value)
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Geo.PostGIS.Geometry.dump(value)
  end

  def graphql_type(_), do: :json

  @doc """
  Formats a point as a comma-separated "latitude,longitude" string — the order
  humans use, which is the reverse of the {longitude, latitude} order the
  coordinates are stored in.
  """
  def lat_lng_string(%Geo.Point{coordinates: {lng, lat}}), do: "#{lat},#{lng}"
  def lat_lng_string(_), do: nil
end

# Without this, rendering a point in a template (e.g. the Ash Admin show page)
# raises Protocol.UndefinedError.
defimpl Phoenix.HTML.Safe, for: Geo.Point do
  def to_iodata(point), do: Orcasite.Types.Geometry.lat_lng_string(point) || ""
end

if Code.ensure_loaded?(Ecto.DevLogger) do
  defimpl Ecto.DevLogger.PrintableParameter, for: Geo.Point do
    def to_expression(point) do
      point
      |> to_string_literal()
      |> Ecto.DevLogger.Utils.in_string_quotes()
    end

    def to_string_literal(point) do
      Geo.WKT.Encoder.encode!(point)
    end
  end
end

# geo only implements Jason.Encoder when Elixir's built-in JSON is missing, but
# Phoenix and AshJsonApi encode with Jason, so rendering a point (e.g. a feed's
# location_point) would raise Protocol.UndefinedError.
defimpl Jason.Encoder,
  for: [
    Geo.Point,
    Geo.PointZ,
    Geo.PointM,
    Geo.PointZM,
    Geo.LineString,
    Geo.LineStringM,
    Geo.LineStringZ,
    Geo.LineStringZM,
    Geo.Polygon,
    Geo.PolygonZ,
    Geo.MultiPoint,
    Geo.MultiPointZ,
    Geo.MultiLineString,
    Geo.MultiLineStringZ,
    Geo.MultiPolygon,
    Geo.MultiPolygonZ,
    Geo.GeometryCollection
  ] do
  def encode(value, opts), do: Jason.Encode.map(Geo.JSON.encode!(value), opts)
end
