defmodule Orcasite.Radio.Seed.Utils do
  def prepare_results(results, resource) do
    results
    |> Enum.map(fn result ->
      convert_result(result, resource)
    end)
  end

  def convert_result(result, resource) do
    result
    |> Enum.flat_map(fn {key, val} ->
      attr = Absinthe.Adapter.Underscore.to_internal_name(key, [])

      cond do
        writable_attr?(resource, attr) ->
          [{attr, val}]

        many_relationship?(resource, attr) ->
          # Recursively underscore has_many relationships
          [{attr, prepare_results(val, relationship_resource(resource, attr))}]

        belongs_to_relationship?(resource, attr) ->
          # Convert single relationship
          [{attr, convert_result(val, relationship_resource(resource, attr))}]

        true ->
          []
      end
    end)
    |> Map.new()
  end

  def writable_attr?(resource, key) do
    Ash.Resource.Info.attribute(resource, key)
    |> case do
      %{writable?: true} -> true
      _ -> false
    end
  end

  def many_relationship?(resource, key) do
    resource
    |> Ash.Resource.Info.relationship(key)
    |> case do
      %{type: type} when type in [:has_many, :many_to_many] -> true
      _ -> false
    end
  end

  def belongs_to_relationship?(resource, key) do
    resource
    |> Ash.Resource.Info.relationship(key)
    |> case do
      %{type: :belongs_to} -> true
      _ -> false
    end
  end

  def relationship_resource(resource, key) do
    resource
    |> Ash.Resource.Info.relationship(key)
    |> Map.get(:destination)
  end
end
