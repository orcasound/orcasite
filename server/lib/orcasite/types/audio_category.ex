defmodule Orcasite.Types.AudioCategory do
  use Ash.Type.Enum, values: [:biophony, :anthrophony, :geophony]

  def graphql_type(_), do: :audio_category
  def graphql_input_type(_), do: :audio_category
end
