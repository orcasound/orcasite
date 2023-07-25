defmodule Orcasite.Radio.Registry do
  use Ash.Registry, extensions: [Ash.Registry.ResourceValidations]

  entries do
    entry Orcasite.Radio.Feed
  end

end
