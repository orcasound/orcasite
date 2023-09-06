Orcasite.Radio.Feed
|> Ash.Query.for_read(:read)
|> Ash.Query.load(:thumb_url)
|> Orcasite.Radio.read!()
|> Enum.map(fn feed ->
  feed
  |> Ash.Changeset.for_update(:update, %{
    image_url: feed.thumb_url
  })
  |> Orcasite.Radio.update!()
end)
