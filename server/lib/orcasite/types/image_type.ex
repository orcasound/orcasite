defmodule Orcasite.Types.ImageType do
  use Ash.Type.Enum, values: [:spectrogram]

  def graphql_type(_), do: :image_type
  def graphql_input_type(_), do: :image_type
end
