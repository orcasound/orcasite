defmodule Orcasite.Radio.Registry do
  use Ash.Registry, extensions: [Ash.Registry.ResourceValidations]

  entries do
    entry Orcasite.Radio.Feed
    entry Orcasite.Radio.Candidate
    entry Orcasite.Radio.Detection
  end

end
