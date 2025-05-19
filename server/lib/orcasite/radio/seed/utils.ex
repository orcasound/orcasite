defmodule Orcasite.Radio.Seed.Utils do
  def prepare_results(results, resource) do
    results
    |> Enum.map(fn result ->
      result
      |> Enum.flat_map(fn {key, val} ->
        attr = Absinthe.Adapter.Underscore.to_internal_name(key, [])

        if Ash.Resource.Info.attribute(resource, attr).writable? do
          [{attr, val}]
        else
          []
        end
      end)
      |> Map.new()
    end)
  end
end
