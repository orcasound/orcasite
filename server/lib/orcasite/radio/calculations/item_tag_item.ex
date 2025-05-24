defmodule Orcasite.Radio.Calculations.ItemTagItem do
  use Ash.Resource.Calculation

  def load(_, _, _) do
    [:bout]
  end

  def strict_loads?, do: false

  @doc """
  Returns the polymorphic item (once we allow tags for different things) see:
  https://hexdocs.pm/ash/polymorphic-relationships.html#defining-the-calculation
  """
  def calculate(records, _, _) do
    Enum.map(records, fn record ->
      cond do
        record.bout ->
          record.bout

        true ->
          nil
      end
    end)
  end
end
