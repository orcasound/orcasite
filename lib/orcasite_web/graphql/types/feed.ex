defmodule OrcasiteWeb.Types.Feed do
  @moduledoc """
  GraphQL types for feeds
  """
  use Absinthe.Schema.Notation

  object :feed do
    field :id, :id
    field :name, :string
    field :node_name, :string
    field :slug, :string
    field :location_point, :json do
      resolve fn %{location_point: point}, _, _  ->
        Geo.JSON.encode(point)
      end
    end
  end
end
