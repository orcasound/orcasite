defmodule OrcasiteWeb.Types.Feed do
  @moduledoc """
  GraphQL types for feeds
  """
  use Absinthe.Schema.Notation

  object :feed do
    field(:id, non_null(:id))
    field(:name, non_null(:string))
    field(:node_name, non_null(:string))
    field(:slug, non_null(:string))

    field :location_point, non_null(:json) do
      resolve(fn
        %{location_point: point}, _, _ when not is_nil(point) ->
          Geo.JSON.encode(point)

        _, _, _ ->
          {:ok, %{}}
      end)
    end

    field :thumb_url, non_null(:string) do
      resolve(fn %{node_name: node_name}, _, _ ->
        {:ok, feed_s3_url(node_name, "thumbnail.png")}
      end)
    end

    field :map_url, non_null(:string) do
      resolve(fn %{node_name: node_name}, _, _ ->
        {:ok, feed_s3_url(node_name, "map.png")}
      end)
    end

    field :intro_html, non_null(:string) do
      resolve(fn %{node_name: node_name}, _, _ ->
        with intro_url <- feed_s3_url(node_name, "intro.html") |> to_charlist(),
             {:ok, {{_, 200, _}, _, resp}} <- :httpc.request(intro_url) do
          intro_html =
            resp
            |> to_string()

          {:ok, intro_html}
        else
          _ -> {:ok, ""}
        end
      end)
    end
  end

  def feed_s3_url(node_name, endpoint) do
    s3_url = Application.get_env(:orcasite, :orcasite_s3_url)
    Path.join([s3_url, node_name, endpoint])
  end
end
