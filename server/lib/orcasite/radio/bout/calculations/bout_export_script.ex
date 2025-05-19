defmodule Orcasite.Radio.Bout.Calculations.BoutExportScript do
  use Ash.Resource.Calculation

  def calculate(records, _opts, _context) do
    # Python script to download the bout's audio and optionally transcode locally
    # with ffmpeg
    records
  end

  def export_script(bout) do
  end
end
