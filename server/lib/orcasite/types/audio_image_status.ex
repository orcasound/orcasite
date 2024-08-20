defmodule Orcasite.Types.AudioImageStatus do
  use Ash.Type.Enum, values: [:new, :processing, :complete]
end
