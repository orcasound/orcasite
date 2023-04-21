# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Orcasite.Repo.insert!(%Orcasite.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.
#
import Ecto.Query
alias Orcasite.Radio.Feed

attrs = %{
  location_point: Geo.WKT.decode!("SRID=4326;POINT(47.60621 -122.33207)"),
  name: "Orcasound Lab (Haro Strait)",
  node_name: "rpi_orcasound_lab",
  slug: "orcasound-lab"
}

from(f in Feed, where: ^Map.to_list(attrs))
|> Orcasite.Repo.exists?()
|> case do
  true ->
    IO.puts("[info] Feed already created")
    {:error, :already_created}

  _ ->
    %Feed{}
    |> Feed.changeset(attrs)
    |> Orcasite.Repo.insert!()
end
