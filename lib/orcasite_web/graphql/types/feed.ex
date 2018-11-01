defmodule OrcasiteWeb.Types.Feed do
  @moduledoc """
  GraphQL types for feeds
  """
  use Absinthe.Schema.Notation

  object :feed do
    field(:id, :id)
    field(:name, :string)
    field(:node_name, :string)
    field(:slug, :string)

    field :location_point, :json do
      resolve(fn
        %{location_point: point}, _, _ when not is_nil(point) ->
          Geo.JSON.encode(point)

        _, _, _ ->
          {:ok, %{}}
      end)
    end

    field :intro_html, :string do
      resolve(fn %{node_name: node_name}, _, _ ->
        with s3_url <- Application.get_env(:orcasite, :orcasite_s3_url),
             intro_url <- Path.join([s3_url, node_name, "intro.html"]) |> to_charlist(),
             {:ok, {{_, 200, _}, _, _} = resp} <- :httpc.request(intro_url) do
          intro_html =
            resp
            |> Tuple.to_list()
            |> List.last()
            |> to_string()

          {:ok, intro_html}
        else
          error -> {:ok, ""}
        end
      end)
    end
  end
end
