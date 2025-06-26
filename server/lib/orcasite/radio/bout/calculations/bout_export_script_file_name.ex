defmodule Orcasite.Radio.Bout.Calculations.BoutExportScriptFileName do
  use Ash.Resource.Calculation

  @impl Ash.Resource.Calculation
  def calculate(records, _opts, _context) do
    Enum.map(records, fn bout ->
      # Use manual calculation to get encoded ids, e.g.
      # "bout_02yvkc7AvrVDIdCChlSwzP"
      "orcasound_download_bout__" <> bout.id <> ".py"
    end)
  end
end
