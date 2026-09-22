defmodule Orcasite.Radio.Seed.Utils do
  @doc """
  Converts production's GraphQL results into inputs for `resource`.

  With an `action`, top-level fields are kept only if that action accepts them.
  Production exposes fields the local action may not take (a feed's
  `maintainerEmails`, for one), and a single unaccepted input fails the whole
  bulk create. Nested relationships are still filtered by writability, since
  the actions that consume them are chosen by the parent action.
  """
  def prepare_results(results, resource, action \\ nil) do
    results
    |> Enum.map(fn result ->
      convert_result(result, resource, action)
    end)
  end

  def convert_result(result, resource, action \\ nil) do
    result
    |> Enum.flat_map(fn {key, val} ->
      attr = Absinthe.Adapter.Underscore.to_internal_name(key, [])

      cond do
        input?(resource, action, attr) ->
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

  defp input?(resource, nil, key), do: writable_attr?(resource, key)

  defp input?(resource, action, key) do
    %{accept: accept, arguments: arguments} = Ash.Resource.Info.action(resource, action)

    key in Enum.map(accept, &to_string/1) or
      key in Enum.map(arguments, &to_string(&1.name))
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
