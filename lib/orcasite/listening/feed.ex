defmodule Orcasite.Listening.Feed do
  use Ecto.Schema
  import Ecto.Changeset


  schema "feeds" do
    field :location_name, :string
    field :name, :string
    field :node_name, :string
    field :location_point, Geo.PostGIS.Geometry

    timestamps()
  end

  @doc false
  def changeset(feed, attrs) do
    feed
    |> cast(attrs, [:name, :node_name, :location_name, :location_point])
    |> validate_required([:name, :node_name, :location_name, :location_point])
  end
end
