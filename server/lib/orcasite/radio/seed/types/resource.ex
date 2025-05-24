defmodule Orcasite.Radio.Seed.Types.Resource do
  use Ash.Type.Enum,
    values: [:feed, :feed_stream, :feed_segment, :candidate, :detection, :audio_image, :bout]

  def to_module(type), do: Orcasite.Radio |> Module.concat(Macro.camelize(to_string(type)))

  def graphql_type(_), do: :seed_resource
end
