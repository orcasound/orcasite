defmodule Orcasite.Radio.Seed.Types.Resource do
  @non_feed_resources [:feed_stream, :feed_segment, :candidate, :detection, :audio_image]

  use Ash.Type.Enum,
    values: [:feed | @non_feed_resources]

  def non_feed_resources(), do: @non_feed_resources

  def to_module(type), do: Orcasite.Radio |> Macro.concat(Macro.camelize(to_string(type)))
end
