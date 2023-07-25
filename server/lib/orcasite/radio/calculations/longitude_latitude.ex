defmodule Orcasite.Radio.Calculations.LongitudeLatitude do
  use Ash.Calculation

  @impl true
  def load(_query, _opts, _context) do
    [:location_point]
  end

  @impl true
  def calculate(records, _opts, _arguments) do
    Enum.map(records, fn %{location_point: %{coordinates: {lng, lat}}} ->
      "#{lng},#{lat}"
    end)
  end
end
