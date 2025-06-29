defmodule Orcasite.Radio.Bout.Calculations.BoutExportJsonFileName do
  use Ash.Resource.Calculation

  @impl Ash.Resource.Calculation
  def calculate(records, _opts, _context) do
    Enum.map(records, fn bout ->
      # Use manual calculation to get encoded ids, e.g.
      # "bout_02yvkc7AvrVDIdCChlSwzP"
      "orcasound_" <> bout.id <> ".json"
    end)
  end
end
