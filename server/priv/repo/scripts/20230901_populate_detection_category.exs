Orcasite.Radio.Detection
|> Ash.Query.for_read(:read)
|> Orcasite.Radio.read!()
|> Enum.map(fn detection ->
  category = case Regex.run(~r/^\[(orca|vessel|other)\]/, detection.description || "") do
    [_full, category | _rest ] -> category
    _ -> nil
  end

  detection
  |> Ash.Changeset.for_update(:update, %{
    category: category
  })
  |> Orcasite.Radio.update!()
end)
