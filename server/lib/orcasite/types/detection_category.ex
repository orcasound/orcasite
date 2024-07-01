defmodule Orcasite.Types.DetectionCategory do
  use Ash.Type.Enum, values: [:whale, :vessel, :other]

  def graphql_type(_), do: :detection_category
  def graphql_input_type(_), do: :detection_category
end
