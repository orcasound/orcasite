defmodule Orcasite.Radio.Registry do
  use Ash.Registry, extensions: [Ash.Registry.ResourceValidations]

  entries do
    entry Orcasite.Radio.Feed
    entry Orcasite.Radio.Candidate
    entry Orcasite.Radio.Detection
    entry Orcasite.Radio.Bout
    entry Orcasite.Radio.FeedStream
    entry Orcasite.Radio.BoutFeedStream
  end

end
