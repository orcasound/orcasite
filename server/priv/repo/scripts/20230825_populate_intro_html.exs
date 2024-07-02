require Ash.Query

alias Orcasite.Radio.Feed

Feed
|> Ash.Query.for_read(:read)
|> Ash.Query.filter(intro_html == "")
|> Ash.read!()
|> Enum.map(fn feed ->
  node_name = feed.node_name
  s3_url = Application.get_env(:orcasite, :orcasite_s3_url)
  endpoint = "intro.html"

  intro_html =
    with intro_url <- Path.join([s3_url, node_name, endpoint]) |> to_charlist(),
         {:ok, {{_, 200, _}, _, resp}} <- :httpc.request(intro_url) do
      intro_html =
        resp
        |> to_string()

      intro_html
    else
      _ -> ""
    end

  feed
  |> Ash.Changeset.for_update(:update, %{
    intro_html: intro_html
  })
  |> Ash.update()
end)
