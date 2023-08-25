defmodule Orcasite.Radio.Calculations.LatLng do
  use Ash.Calculation

  @impl true
  def load(_query, _opts, _context) do
    [:location_point]
  end

  @impl true
  def calculate(records, opts, _arguments) do
    return_type = Keyword.get(opts, :return_type, :map)
    Enum.map(records, fn %{location_point: %{coordinates: {lng, lat}}} ->
      case return_type do
        :map -> %{lat: lat, lng: lng}
        :string -> "#{lat},#{lng}"
        _ -> %{lat: lat, lng: lng}
      end
    end)
  end
end
