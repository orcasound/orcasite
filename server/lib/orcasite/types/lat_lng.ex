defmodule Orcasite.Types.LatLng do
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        lat: [type: :float, allow_nil?: false],
        lng: [type: :float, allow_nil?: false]
      ]
    ]

  def graphql_type(_), do: :lat_lng
end
