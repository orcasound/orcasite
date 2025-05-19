defmodule Orcasite.Radio.Seed.Utils do
  def local_to_prod_feed_id(feed_id) do
    %{slug: feed_slug} = Orcasite.Radio.Feed |> Ash.get!(feed_id, authorize?: false)
    %{"id" => id} = Orcasite.Radio.GraphqlClient.get_feed(feed_slug)
    id
  end

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
