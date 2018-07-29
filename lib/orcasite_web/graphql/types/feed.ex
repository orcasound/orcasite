defmodule OrcasiteWeb.Types.Feed do
  @moduledoc """
  GraphQL types for feeds
  """
  use Absinthe.Schema.Notation

  object :feed do
    field :id, :id
    field :location_name, :string
    field :node_name, :string
    field :location_point, :json do
      resolve fn %{location_point: point}, _, _  ->
        Geo.JSON.encode(point)
      end
    end
  end
end
