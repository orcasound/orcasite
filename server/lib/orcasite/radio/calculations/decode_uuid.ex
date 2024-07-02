defmodule Orcasite.Radio.Calculations.DecodeUUID do
  use Ash.Resource.Calculation

  @impl true
  def load(_query, _opts, _context) do
    [:id]
  end

  @impl true
  def calculate(records, _opts, _arguments) do
    Enum.map(records, fn record ->
      decode(record)
    end)
  end

  def decode(record) do
    [_, encoded_id] =
      record
      |> Map.get(:id)
      |> String.split("_")

    with {:ok, uuid} <- AshUUID.Encoder.decode(encoded_id) do
      uuid
    else
      _ -> nil
    end
  end
end
