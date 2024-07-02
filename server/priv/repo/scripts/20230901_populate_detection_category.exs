Orcasite.Radio.Detection
|> Ash.Query.for_read(:read)
|> Ash.read!()
|> Enum.map(fn detection ->
  category = case Regex.run(~r/^\[(orca|vessel|other)\]/, detection.description || "") do
    [_full, category | _rest ] -> category
    _ -> nil
  end

  detection
  |> Ash.Changeset.for_update(:update, %{
    category: category
  })
  |> Ash.update!()
end)
