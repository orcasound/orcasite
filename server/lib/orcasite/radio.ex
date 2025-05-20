defmodule Orcasite.Radio do
  use Ash.Domain, extensions: [AshAdmin.Domain, AshGraphql.Domain, AshJsonApi.Domain]

  resources do
    resource Orcasite.Radio.Feed
    resource Orcasite.Radio.Candidate
    resource Orcasite.Radio.Detection
    resource Orcasite.Radio.Bout
    resource Orcasite.Radio.FeedStream
    resource Orcasite.Radio.BoutFeedStream
    resource Orcasite.Radio.FeedSegment
    resource Orcasite.Radio.AudioImage
    resource Orcasite.Radio.AudioImageFeedSegment
    resource Orcasite.Radio.Tag
    resource Orcasite.Radio.ItemTag
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
