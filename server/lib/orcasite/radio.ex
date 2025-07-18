defmodule Orcasite.Radio do
  use Ash.Domain, extensions: [AshAdmin.Domain, AshGraphql.Domain, AshJsonApi.Domain]

  resources do
    resource Orcasite.Radio.AudioImage
    resource Orcasite.Radio.AudioImageFeedSegment
    resource Orcasite.Radio.Bout
    resource Orcasite.Radio.BoutFeedStream
    resource Orcasite.Radio.Candidate
    resource Orcasite.Radio.Detection
    resource Orcasite.Radio.Feed
    resource Orcasite.Radio.FeedListenerCount
    resource Orcasite.Radio.FeedSegment
    resource Orcasite.Radio.FeedStream
    resource Orcasite.Radio.ItemTag
    resource Orcasite.Radio.Seed
    resource Orcasite.Radio.Tag
  end

  admin do
    show? true
  end

  graphql do
  end

  json_api do
    log_errors? true
  end
end
