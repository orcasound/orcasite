defmodule Orcasite.Radio.Detection.Types.Source do
  use Ash.Type.Enum, values: [:human, :machine]

  def graphql_type(_), do: :detection_source
end
